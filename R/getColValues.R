#' Retrieves values from one column of a quitte object
#'
#' Retrieves values from one column of a quitte object
#'
#'
#' @param df quitte object
#' @param colVar name of the column of interest
#' @return a  vector containing the values of the column without duplicates. The class of the returned vector is either numeric or character
#' @author Antoine Levesque
#' @examples
#' data(mtcars)
#' getColValues(mtcars,"mpg")
#' @export



getColValues <- function(df, colVar){
  if (is.numeric(df[[colVar]])) {
    tmp = unique(df[[colVar]])
  }else{
  tmp = unique(as.character(df[[colVar]]))
  }
  return(tmp)
}