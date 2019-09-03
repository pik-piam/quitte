#'
#' Sum over one dimension of a data frame
#'
#' \code{sum_total()} is a short-hand function to calculate and insert the
#' (weighted) sum of a extensive (intensive) category in a data frame.
#'
#' @param data a data frame
#' @param group column for which the sum is to be calculated
#' @param value column of the numbers to be summed
#' @param name entry in column \code{group} for the sum; defaults to
#'        \code{"Total"}
#' @param na.rm \code{logical.} Should missing values (including NaN) be removed
#'        (default)?
#' @param weight column of the weights to be applied, if any
#' @return a data frame
#' @author Michaja Pehl
#' @examples
#'
#' require(dplyr)
#'
#' (d <- expand.grid(
#'     UPPER  = LETTERS[1:2],
#'     lower  = letters[24:26],
#'     number = 1:2
#' ) %>%
#'         arrange(UPPER, lower, number) %>%
#'         mutate(value = c(1:6, NA, 8:12)))
#'
#' sum_total(d, UPPER)
#'
#' sum_total(d, lower, name = 'sum over lower', na.rm = FALSE)
#'
#' (e <- data_frame(
#'     item = c('large', 'medium', 'small'),
#'     specific.value = c(1, 10, 100),
#'     size = c(1000, 100, 1)))
#'
#' sum_total(e, item, value = specific.value, name = 'Average', weight = size)
#'
#' @export
sum_total <- function(data, group, value = NA, name = "Total", na.rm = TRUE,
                      weight = NA) {

    if (!is.null(group <- substitute(group)))
        if (!is.logical(group))
            group <- deparse(group)

    if (!is.null(value <- substitute(value)))
        if (!is.logical(value))
            value <- deparse(value)

    if (!is.null(weight <- substitute(weight)))
        if (!is.logical(weight))
            weight <- deparse(weight)

    sum_total_(data, group, value, name, na.rm, weight)
}

#' @export
#' @rdname sum_total
sum_total_ <- function(data, group, value = NA, name = "Total", na.rm = TRUE,
                       weight = NA) {

    if (is.na(value))
        value = "value"

    # guardians
    if (!is.data.frame(data))
        stop("only works with data frames")

    if (!(group %in% colnames(data)))
        stop("No column '", group, "' in data frame")

    if (!(value %in% colnames(data)))
        stop("No column '", value, "' in data frame")

    if (!is.na(weight) & !(weight %in% colnames(data)))
        stop('No column \'', weight, '\' in data frame')

    .colnames <- colnames(data)
    .groups <- setdiff(.colnames, c(group, value, weight))

    .groups.old <- c()
    for (i in 1:length(groups(data)))
        .groups.old <- c(.groups.old, deparse(groups(data)[[i]]))

    if (is.na(weight)) {
        summarise.list <- setNames(
            list(lazyeval::interp(~sum(value, na.rm = na.rm),
                                  value = as.name(value))),
            value)
    } else {
        summarise.list <- setNames(
            list(
                lazyeval::interp(
                    `_obj` = ~( sum(value * weight, na.rm = na.rm)
                              / sum(weight, na.rm = na.rm)),
                    value  = as.name(value),
                    weight = as.name(weight)),
                lazyeval::interp(
                    `_obj` = ~sum(weight, na.rm = na.rm),
                    weight = as.name(weight))),
            c(value, weight))
    }

    mutate.list <- setNames(
        list(lazyeval::interp(~name, name = as.character(name))),
        group)

    data <- data %>%
        filter_(lazyeval::interp(~group != name, group = as.name(group),
                                 name = name))

    .data <- rbind(
        data %>%
            ungroup(),
        data %>%
            group_by_(.dots = .groups, add = TRUE) %>%
            summarise_(.dots = summarise.list) %>%
            mutate_(.dots = mutate.list) %>%
            select_(.dots = .colnames) %>%
            ungroup()
    ) %>%
        arrange_(.dots = c(.groups, group))

    if (length(groups(data)) > 0)
        .data <- .data %>%
        group_by_(.groups.old)

    return(.data)
}
