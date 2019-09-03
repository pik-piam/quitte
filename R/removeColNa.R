#' Removes all NA columns of a data frame
#'
#' \code{removeColNa()} Removes all columns of a data frame for which all entries are NA 
#'
#' @param df a data frame
#' @return a data frame
#' @author Antoine Levesque
#' @examples
#' df <- data.frame(
#'     character = letters[1:5],
#'     factor = as.factor(LETTERS[1:5]),
#'     value = 1:5,
#'     unit = NA,
#'     stringsAsFactors = FALSE)
#' str(df)
#' str(removeColNa(df))
#' @export
#' 

removeColNa <- function(df){

return(df[colSums(!is.na(df)) > 0])
}