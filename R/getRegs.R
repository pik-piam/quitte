#' Retrieves regions from a quitte object
#'
#' Retrieves regions from a quitte object
#'
#'
#' @param df quitte object
#' @return a character vector containing the values of the column without
#'   duplicates
#' @author Antoine Levesque
#' @examples
#'
#'   \dontrun{
#'     getRegs(dataframe)
#'   }
#' @export



getRegs <- function(df){
  tmp = getColValues(df,"region")
  return(tmp)
}
