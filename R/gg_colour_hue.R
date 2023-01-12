#' Get n Colours
#'
#' Get n colours, evenly spaced along the colour wheel.  Just like the ones
#' [ggplot2::scale_colour_hue()] is using.
#'
#' @param n Either the number of colours to generate, or a character vector
#' which will be used as names for the returned colours.
#'
#' @return A vector of character strings which can be used as color
#' specifications by R graphics functions.  The vector is named if n is a
#' character vector.
#'
#' @examples
#' gg_colour_hue(5)
#' gg_colour_hue(letters[1:3])
#'
#' @importFrom grDevices hcl
#' @importFrom stats setNames
#'
#' @export
gg_colour_hue <- function(n) {
    if (is.numeric(n) && all(as.integer(n) == n)) {
        head(hcl(h = seq(15, 375, length = n + 1), l = 65, c = 100), -1)
    } else if (is.character(n)) {
        setNames(
            head(hcl(h = seq(15, 375, length = length(n) + 1), l = 65, c = 100),
                 -1),
            n)
    } else {
        stop('n must be integer or character vector')
    }
}
