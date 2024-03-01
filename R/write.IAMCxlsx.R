
#' Write .xlsx file
#'
#' Write a `.xlsx` file in line with IAMC standard.
#'
#' @md
#' @param x A [`quitte`] data frame.
#' @param path Path or connection to write to.
#' @param append Overwrite existing files (`FALSE`, default), or append to them
#'   (`TRUE`).
#'
#' @author Michaja Pehl, Oliver Richters
#'
#' @examples
#' write.IAMCxlsx(quitte_example_data, tempfile())
#'
#' @importFrom dplyr mutate rename_with select
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect matches
#' @importFrom stringr str_to_title
#' @importFrom writexl write_xlsx

#' @export
write.IAMCxlsx <- function(x, path, append = FALSE) {
    x <- as.quitte(x)
    if (nrow(x) == 0) warning("Writing empty data frame to ", path)
    default_columns <- c('Model', 'Scenario', 'Region', 'Variable', 'Unit',
                         'Period', 'Value')

    . <- NULL

    if (append && file.exists(path)) {
        x <- rbind(read.quitte(path), x)
    }

    # make column names upper-case
    x <- x %>%
        rename_with(.fn = str_to_title,
                    .cols = matches('^[A-Za-z][A-Za-z0-9_]*$')) %>%
        select(setdiff(default_columns, last(default_columns)),
               setdiff(colnames(.), default_columns),
               last(default_columns)) %>%
               filter(is.finite(.data$Value), '' != .data$Value) %>%
               pivot_wider(names_from = 'Period', names_sort = TRUE, values_from = 'Value', values_fill = NA)

    write_xlsx(list("data" = x), path)
}
