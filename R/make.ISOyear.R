#' speedily converting years to POSIXct values
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated because the [ISOyear()] function can be used
#' directly.
#'
#' @param years ignored.
#'
#' @return The [ISOyear()] function.
#'
#' @examples
#' ISOyear <- make.ISOyear()
#' ISOyear(c(2005, 2010, 2100, 1900))
#' # ->
#' ISOyear(c(2005, 2010, 2100, 1900))
#'
#' @keywords internal
#'
#' @export
make.ISOyear <- function(years) {
    lifecycle::deprecate_warn(
        when = '0.3108.0', what = 'make.ISOyear()',
        details = 'The function ISOyear() can be used directly.')
    return(ISOyear)
}
