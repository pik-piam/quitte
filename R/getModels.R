#' Retrieves models from a quitte object
#'
#' Retrieves models from a quitte object
#'
#'
#' @param df quitte object
#' @return a character vector containing the values of the column without duplicates
#' @author Antoine Levesque
#' @examples
#'
#'   \dontrun{
#'     getPeriods(dataframe)
#'   }
#' @export



getModels <- function(df){
  tmp = getColValues(df,"model")
  return(tmp)
}