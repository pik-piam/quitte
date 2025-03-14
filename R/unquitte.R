#' Remove quitte class atribute
#'
#' @param x Any object.
#'
#' @returns `x`, with the `quitte` class attribute removed (if any).
#'
#' @examples
#' quitte_example_data |> class()
#' quitte_example_data |> unquitte() |> class()
#' mtcars |> class()
#' mtcars |> unquitte() |> class()

#' @export
unquitte <- function(x)
{
    `class<-`(x, setdiff(class(x), 'quitte'))
}
