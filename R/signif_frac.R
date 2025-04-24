#' Round fractions only
#'
#' Round numbers to significant digits, preserving integer parts.
#'
#' @param x A numeric vector.
#' @param digits Number of significant digits, defaults to 3.
#'
#' @return A numeric vector.
#'
#' @examples
#' set.seed(0)
#' x <- runif(8, -1, 1) * 10 ^ seq.int(5, -2)
#'
#' data.frame(number           = x,
#'            formatted        = sprintf('%g', x),
#'            `w/ signif`      = sprintf('%g', signif(x, 3)),
#'            `w/ signif_frac` = sprintf('%g', signif_frac(x)),
#'            check.names = FALSE)
#'
#' @importFrom glue glue

#' @export
signif_frac <- function(x, digits = 3)
{
    if (!is.numeric(x))
        stop(glue('x must be numeric, but is {class(x)}.'))

    sapply(x, \(x) signif(x, max(digits, ceiling(log10(abs(x))))))
}
