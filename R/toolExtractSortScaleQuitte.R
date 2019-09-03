#' prepare data for plots
#'
#' @param x dataframe to prepare
#' @param scen scenario to select
#' @param vars variables to select
#' @param var.scaling scaling of the variables, default=1
#' @param regi region to select, default="World"
#' @param prd period to select, default=getPeriods(x)
#' @author Gunnar Luderer, Lavinia Baumstark
#' @examples
#'
#'   \dontrun{
#'     p <- toolExtractSortScaleQuitte(x,scen=c("BAU"),
#'                                    vars=c("Emi|CO2","FE|Industry"),
#'                                    regi=c("EUR","LAM"),prd=c(2005,2030,2050))
#'   }
#'
#'
#'
#' @export
toolExtractSortScaleQuitte <- function(x,scen,vars,
                                       var.scaling=1,
                                       regi=c("World"),
                                       prd=getPeriods(x)) {

  x <- x %>%
    filter_(~region %in% regi,~variable %in% vars,~scenario %in% scen,~period %in% prd) %>%
    mutate_(variable=~factor(x=variable,levels=vars,ordered=TRUE) ) %>%
    mutate_(value=~value * var.scaling ) %>%
    factor.data.frame
  

  x$variable <- factor(x$variable,levels=vars,ordered=TRUE)

  #get rid of NAs
  x <- x[!is.na(x$value),]

  x <- as.quitte(x)
  return(x)

}

