#' Calculate mitigation costs
#'
#' Calculate mitigation costs
#'
#'
#' @param data quitte object
#' @param scenBau baseline scenario name
#' @param scenPol policy scenario name
#' @param yearFrom the startyear
#' @param yearTo the endyear
#' @param nameVar Name of the variable containing consumption. Defaults to
#' "Consumption"
#' @param nameDisrate Name of the variable for the discount rate, only needed
#' if discount=endo. Defaults to "Interest Rate t/(t-1)|Real"
#' @param discount discount rate - exogenous only for now
#' @return regional mitigation costs (quitte object)
#' @author Anselm Schultes
#' @examples
#'
#'   \dontrun{
#'     calcMitigationCost(qd,"BAU","POL")
#'   }
#'
#' @export
calcMitigationCost = function(data,scenBau,scenPol,
                              yearFrom=2010,yearTo=2100,
                              nameVar='Consumption',
                              nameDisrate='Interest Rate t/(t-1)|Real',
                              discount=0.05){

  if(! nameVar %in% unique(data$variable)) stop(paste('Variable ',nameVar,' not found in data.'))

  #takes quitte data, scenario names, start- and end-year
  #select data
  data = data[data$scenario %in% c(scenBau,scenPol)
          & data$variable %in% c(nameVar,nameDisrate)
          & data$period >= yearFrom
          & data$period <= yearTo,]
  #replace POL interest rate with BAU interest rate:
  if(!is.numeric(discount)){
    if(! nameDisrate %in% unique(data$variable)) {
      stop(paste('Variable ',nameDisrate,' not found in data. Stoping.'))
    }
    data[data$variable == nameDisrate & data$scenario == scenPol,'value'] =
      data[data$variable == nameDisrate & data$scenario == scenBau,'value']
  }

  #calculate discounted aggregate values
  tmp = data.frame(calcCumulatedDiscount(data,nameVar=nameVar,
                                         nameDisrate=nameDisrate,
                                         discount=discount))
  #calculate mitigaton costs
  res = tmp %>%
   filter(!!sym('period') == yearTo) %>%
    group_by(!!sym('model'), !!sym('region')) %>%
    arrange(!!sym('scenario') == scenPol) %>%
    summarise(
      !!sym('value') := 100 * (1 - !!sym('value')[2] / !!sym('value')[1])) %>%
    ungroup()
  res$scenario = scenPol
  res$variable = 'Mitigation cost'
  res$unit = 'pp'

  return(as.quitte(data.frame(res)))

}

