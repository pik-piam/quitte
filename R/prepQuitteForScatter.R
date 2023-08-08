## Function Name ################################################
## prepQuitteForScatter                                        ##
## Description ##################################################
## Aim    : This function prepares a Quitte for scatter plots  ##
## Author : N. Bauer (nico.bauer@pik-potsdam.de)               ##
## History:                                                    ##
##   - 2014-12-11: Creation                                    ##
## End of Description ###########################################



#' selects two variables from a long format Quitte and puts them into a wide
#' format Quitte
#'
#' QuitteIn contains two variables containedin varNames that should be plotted
#' in a scatter plot. The functions forms the new QuitteOut with the variables
#' x and y. QuitteOut can be used in ggplot with plotting x and y. The unit
#' needs to be replaced by None.
#'
#'
#' @param quitteIn Quitte with original data
#' @param varNames Vector with two variable names that must be contained in
#' QuitteIn$variable
#' @return quitte object
#' @author Nico Bauer, Anselm Schultes, Jerome Hilaire
#' @examples
#'
#'   \dontrun{
#'     quitteOut <- prepQuitteForScatter(quitteIn, c('Emissions|CO2', 'Price|Carbon'))
#'   }
#'
#' @importFrom reshape2 dcast
#'
#' @export
prepQuitteForScatter <- function(quitteIn , varNames) {
  # get rid of all units and replace them with "none"
  quitteIn$unit <- 'None'

  # select the variables in varNames (this should be two) and put them in wide format
  quitteOut <- dcast(quitteIn[quitteIn$variable %in% varNames, ], ... ~ variable)

  # rename the variables with "x" and "y" for further use in more general plotting functions
  names(quitteOut)[which(names(quitteOut) == varNames[1])] <- "x"
  names(quitteOut)[which(names(quitteOut) == varNames[2])] <- "y"

  return(quitteOut)
}
