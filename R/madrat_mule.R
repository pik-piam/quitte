#' Madrat Mule
#'
#' Convert _anything_ to a [`magpie`][magclass::magclass] object and back to
#' traffic data across `madrat` borders.
#'
#' @md
#' @param x Anything.
#'
#' @return A [`magpie`][magclass::magclass] object containing `x` (in unusable
#'   form), or the original `x` if a [`magpie`][magclass::magclass] object was
#'   passed.
#'
#' @author Michaja Pehl
#'
#' @examples
#' str(x <- madrat_mule(quitte_example_data))
#' madrat_mule(x)
#'
#' @importFrom magclass as.magpie

#' @export
madrat_mule <- function(x)
{
    if (!inherits(x, 'magpie')) {
        return(as.magpie(as.integer(serialize(x, NULL))))
    }
    else {
        return(unserialize(as.raw(as.vector(x))))
    }
}
