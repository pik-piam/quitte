#' Uses mutate based on a character vector#'
#'
#'
#' @param df a data frame
#' @param s a character string containing the formula to be applied in mutate
#'
#' @return A data frame transformed with the mutate function
#' @examples
#' df = data.frame(x = c(1,2),
#'                 y = c(3,4))
#' form = "z = x + y"
#' mutate_text(df,form)
#'
#' @author Antoine Levesque
#'
#' @export

mutate_text = function(df, s){
  q = quote(mutate(df, z = s))
  eval(parse(text=sub("z = s", s, deparse(q))))
}
