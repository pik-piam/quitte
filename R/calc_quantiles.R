#' Sample Quantiles
#'
#' This is a wrapper function for \code{\link[stats]{quantile}} or easy use with
#' data frames.
#'
#' @author Michaja Pehl
#'
#' @param .data a data frame, possibly grouped
#' @param value column name for which sample quantiles should be calculated
#' @param probs named numeric vector of probabilities with values in
#'              \eqn{[0, 1]}.
#' @param na.rm logical; if true, any \code{\link{NA}} and \code{\link{NaN}}s
#'              are removed from x before the quantiles are computed.
#' @param type an integer between 1 and 9 select one of the nine quantile.
#'             algorithms detailed in \code{\link[stats]{quantile}} to be used.
#'
#' @return a data frame.
#'
#' @examples
#' require(dplyr)
#' require(tidyr)
#' data <- tibble(group = rep(c("A", "B"), 10),
#'                    value = 1:20) %>%
#'                    arrange(group) %>%
#'                    group_by(group)
#'
#' data %>%
#'  calc_quantiles() %>%
#'  spread(quantile, value)
#'
#' @importFrom tibble tibble_
#'
#' @export
calc_quantiles <- function(.data,
                           value = NA,
                           probs = c("q0"   = 0,
                                     "q25"  = 0.25,
                                     "q50"  = 0.5,
                                     "q75"  = 0.75,
                                     "q100" = 1),
                           na.rm = TRUE,
                           type  = 7) {

    value <- deparse(substitute(value))
    if ("NA" == value)
        value <- "value"

    calc_quantiles_(.data, value, probs, na.rm, type)
}

#' @export
#' @rdname calc_quantiles
calc_quantiles_ <- function(.data,
                            value = "value",
                            probs = c("q0"   = 0,
                                      "q25"  = 0.25,
                                      "q50"  = 0.5,
                                      "q75"  = 0.75,
                                      "q100" = 1),
                            na.rm = TRUE,
                            type  = 7) {

    # guardians
    if (!is.data.frame(.data))
        stop("only works for data frames")

    if (!value %in% colnames(.data))
        stop("no column '", value, "'")

    rename.list <- list(lazyeval::interp(~value))
    names(rename.list) <- value

    .data %>%
        do(tibble_(
            list("quantile" = lazyeval::interp(~factor(names(probs),
                                             levels = names(probs))),
                 "value"    = lazyeval::interp(~quantile(getElement(., value),
                                               probs, na.rm, names = FALSE,
                                               type))))
        ) %>%
        rename_(.dots = rename.list)
}
