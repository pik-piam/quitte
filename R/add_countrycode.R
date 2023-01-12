#' Add country code
#'
#' Wrapper function for [countrycode::countrycode()] enabling piped
#' execution.
#'
#' @param data A data frame.
#' @param ... Key-value pairs for NSE of `origin` and `destination`
#'        parameters.
#' @param warn Prints unique elements from sourcevar for which no match was
#'             found.
#' @param na.rm If `TRUE`, remove ambiguously matched rows.
#' @param origin Named scalar linking source column to source coding scheme. See
#'        [countrycode::countrycode()] for details.
#' @param destination Named scalar linking destination column name to
#'        destination coding scheme. See [countrycode::countrycode()]
#'        for details.
#'
#' @return A data frame.
#'
#' @author Michaja Pehl
#'
#' @importFrom countrycode countrycode
#'
#' @examples
#' library(dplyr)
#' data <- tibble(
#'     country = c('Belgium', 'Narnia', 'Russia', 'Botswana'),
#'     data    = 1:4)
#'
#' data %>% add_countrycode(country = country.name, m49.code = un)
#' data %>% add_countrycode_(c('country' = 'country.name'), 'iso3c',
#'                           warn = FALSE, na.rm = TRUE)

#' @export
add_countrycode <- function(data, ..., warn = TRUE, na.rm = FALSE) {
    dots <- lazyeval::lazy_dots(...)

    source.type   <- as.character(dots[[1]]$expr)
    source.column <- ifelse('' == names(dots[1]), source.type, names(dots[1]))

    destination.type   <- as.character(dots[[2]]$expr)
    destination.column <- ifelse('' == names(dots[2]), destination.type,
                                 names(dots[2]))

    add_countrycode_(data,
                     stats::setNames(source.type, source.column),
                     stats::setNames(destination.type, destination.column),
                     warn = warn, na.rm = na.rm)
}

#' @export
#' @rdname add_countrycode
add_countrycode_ <- function(data, origin, destination, warn = TRUE,
                             na.rm = FALSE) {

    source.type        <- stats::setNames(origin, NULL)
    source.column      <- ifelse(is.null(names(origin)), source.type,
                                 names(origin))

    destination.type   <- stats::setNames(destination, NULL)
    destination.column <- ifelse(is.null(names(destination)), destination.type,
                                 names(destination))

    data <- data %>%
        mutate(
            !!sym(destination.column) := countrycode(
                sourcevar   = !!sym(source.column),
                origin      = source.type,
                destination = destination.type,
                warn = warn))

    if (na.rm) {
        data <- data %>%
            filter(!is.na(!!sym(destination.column)))
    }

    return(data)
}
