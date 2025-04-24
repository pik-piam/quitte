#' Calculate the mode of a sample
#'
#' @param v A vector.
#'
#' @return The mode, or a vector of modes if the sample is multi-modal.
#'
#' @author Michaja Pehl
#'
#' @examples
#' calc_mode(c(1, 1, 100))
#' calc_mode(c('a', 'a', 'b', 'c', 'c'))

#' @export
calc_mode <- function(v)
{
    u <- unique(v)
    t <- tabulate(match(v, u))
    u[which(max(t) == t)]
}
