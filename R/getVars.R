#' Retrieves variable names from a quitte object
#' 
#' Retrieves variable names from a quitte object 
#' 
#' 
#' @param df quitte object
#' @return a character vector containing the values of the column without duplicates
#' @author Antoine Levesque
#' @examples
#' 
#'   \dontrun{
#'     getVars(dataframe)
#'   }
#' @export



getVars <- function(df){
  tmp = getColValues(df,"variable")
  return(tmp)
}