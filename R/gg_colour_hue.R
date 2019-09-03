#' Get n Colours
#'
#' Get n colours, evenly spaced along the colour wheel.  Just like the ones
#' \code{\link[ggplot2]{scale_colour_hue}} is using.
#'
#' @param n Number of colours.
#'
#' @return A vector of character strings which can be used as color
#' specifications by R graphics functions.
#'
#' @examples
#' gg_color_hue(5)
#'
#' @importFrom grDevices hcl
#'
#' @export
gg_colour_hue <- function(n) {
    head(hcl(h = seq(15, 375, length = n + 1), l = 65, c = 100), -1)
}
