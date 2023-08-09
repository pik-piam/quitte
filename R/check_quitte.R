#' Check IAMC-style data frame for inconsistencies.
#'
#' Check an IAMC-style data frame to see if values across variables and regions
#' sum up to the totals specified within the data frame.
#'
#' @md
#' @param quitte IAMC-style data frame.
#' @param check_variables List, string or file of variables to check.
#' @param check_regions List, string or file of regions to check.
#'
#' @return A data frame of all entries that did not match.
#'
#' @author Michaja Pehl
#'
#' @details Checking is performed for all variables and regions in
#' `check_variables` and `check_regions`, which can be passed as a list of
#' format
#' ```
#' list("sum" = c("summand1", "summand2", ...))
#' ```
#' a character string of format
#' ```
#' sum1
#' summand1a
#' summand1b
#'
#' sum2
#' summand2a
#' ...
#' ```
#' or as the path to a text file with this format.
#'
#' If checking should be performed for variables or regions that are neither sum
#' nor summand (e.g., the variable 'GDP' should be summed across regions, but is
#' itself not a sum of other variables), include them as sum and their only
#' summand in the respective list (i.e., `list("GDP" = "GDP")` or as a double
#' line in the character string or file.
#'
#' If `check_regions` is `NULL`, variables are check across all regions in
#' `quitte`.
#'
#' @examples
#' require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
#' quitte <- rbind(
#'     data.frame(
#'         model    = "REMIND",
#'         scenario = "Baseline",
#'         region   = c("World", "USA", "EUR"),
#'         variable = "GDP",
#'         unit     = "US$2005",
#'         period   = 2005,
#'         value    = c(3, 1, 1)
#'     ),
#'
#'     data.frame(
#'         model    = "REMIND",
#'         scenario = "Baseline",
#'         region   = "ROW",
#'         variable = c("FE|Total", "FE|Solids", "FE|Electricity"),
#'         unit     = "EJ/a",
#'         period   = 2005,
#'         value    = c(3, 1, 1)
#'     )
#' )
#'
#' check_variables <- list(
#'     "FE|Total" = c("FE|Solids", "FE|Electricity"),
#'     "GDP"      = "GDP")
#'
#' check_regions <- paste0("World\nUSA\nEUR\n\nROW\nROW")
#'
#' print(quitte)
#' print(check_variables)
#' cat(check_regions)
#'
#' check_quitte(quitte, check_variables, check_regions)
#'
#' @importFrom lazyeval lazy_dots
#' @importFrom stats na.omit
#'
#' @export check_quitte
check_quitte <- function(quitte, check_variables, check_regions = NULL) {

    .getAllNames <- function(l) {
        union(
            l %>%
                names(),

            l %>%
                unlist() %>%
                na.omit() %>%
                as.character()
        ) %>%
            unique()
    }

    .str2lst <- function(s) {
        s <- s %>%
            strsplit("\n\n") %>%
            getElement(1)

        l <- sub("^[^\n]*\n", "", s) %>%
            strsplit("\n")

        names(l) <- sub("^([^\n]*)\n.*$", "\\1", s)

        return(l)
    }

    .summarise.expression <- list(interp(~sum(value)))
    names(.summarise.expression) <- "sum.value"

    # if NULL, use all regions in quitte
    if (is.null(check_regions)) {
        all_regions <- as.character(unique(quitte$region))

        # if string
    } else if (is.character(check_regions)) {

        # if file, read string
        if (file.access(check_regions, 4) != -1)
            check_regions <- readLines(check_regions) %>%
                paste(collapse = "\n")

        # convert string to list
        check_regions <- .str2lst(check_regions)

        all_regions <- .getAllNames(check_regions)
    } else if (is.list(check_regions)) {
        all_regions <- .getAllNames(check_regions)
    } else {
        stop("Can't handle check_regions")
    }

    if (is.character(check_variables)) {

        if (file.access(check_variables, 4) != -1)
            check_variables <- readLines(check_variables) %>%
                paste(collapse = "\n")

        check_variables <- .str2lst(check_variables)
    }
    else if (!is.list(check_variables)) {
        stop("Can't handle check_variables")
    }

    quitte <- quitte %>%
        mutate(!!sym('region') := as.character(!!sym('region')),
               !!sym('variable') := as.character(!!sym('variable')))

    variable.error <- inner_join(
        quitte %>%
            filter(!!sym('region') %in% all_regions,
                   !!sym('variable') %in% names(check_variables)),

        inner_join(
            quitte %>%
                filter(!!sym('region') %in% all_regions),

            suppressWarnings(
                check_variables %>%
                    as.data.frame(optional = TRUE) %>%
                    pivot_longer(check_variables %>%
                                     as.data.frame(optional = TRUE) %>%
                                     colnames(),
                                 names_to = 'name', values_to = 'item')
            ) %>%
                distinct(),

            by = c("variable" = "item")
        ) %>%
            group_by(!!!syms(c("model", "scenario", "region", "period",
                               "name"))) %>%
            summarise(!!sym('sum.value') := sum(!!sym('value'))) %>%
            ungroup(),

        by = c("model", "scenario", "region", "period", "variable" = "name")
    ) %>%
        filter(!!sym('sum.value') != !!sym('value'))

    if (is.null(check_regions)) {
        region.error <- NULL
    } else {
        region.error <- inner_join(
            quitte %>%
                filter(!!sym('variable') %in% .getAllNames(check_variables),
                       !!sym('region') %in% names(check_regions)),

            inner_join(
                quitte %>%
                    filter(!!sym('variable') %in% .getAllNames(check_variables)),

                suppressWarnings(
                    check_regions %>%
                        as.data.frame(optional = TRUE) %>%
                        pivot_longer(check_regions %>%
                                         as.data.frame(optional = TRUE) %>%
                                         colnames(),
                                     names_to = 'name', values_to = 'item')
                ) %>%
                    distinct(),

                by = c("region" = "item")
            ) %>%
                group_by(!!!syms(c("model", "scenario", "variable", "period",
                                   "name"))) %>%
                summarise(!!sym('sum.value') := sum(!!sym('value'))) %>%
                ungroup(),

            by = c("model", "scenario", "region" = "name", "variable", "period")
        ) %>%
            filter(!!sym('sum.value') != !!sym('value'))
    }

    rbind(variable.error, region.error) %>%
        distinct() %>%
        return()
}
