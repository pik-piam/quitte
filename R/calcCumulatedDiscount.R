#' Calculates the cumulated discounted time series
#'
#' Discount and cumulated a times series - gives the time series of the net
#' present value (NPV). Baseyear for the NPV is the first period.
#'
#'
#' @param data a quitte object containing consumption values - consumption has
#' to be named "Consumption"
#' @param nameVar name of the variable to be cumulated (and discounted)
#' @param nameDisrate Name of the variable containing the discount rate
#' @param discount The discount rate: either a numeric value, or 'BAU' to
#' choose the discount rate supplied in nameDisrate
#' @param fixYear From the discounted time series, substract the value in year
#' fixYear, if fixYear is not 'none'
#' @return cumulated discounted values for each scenario, model, region (quitte
#' object)
#' @author Anselm Schultes
#' @examples
#'
#'   \dontrun{
#'     erg <- calcCumulatedDiscount(data, disRate=0.03)
#'   }
#'
#' @export
calcCumulatedDiscount = function(data,
                                 nameVar='Consumption',
                                 nameDisrate='Interest Rate t/(t-1)|Real',
                                 discount=0.05,
                                 fixYear='none'){
  # this functions implements the functionality found here (only for options CumMode=1, BaseMode=1, DisMode=1):
  # http://localhost:8836/projects/remind-matlab/repository/entry/Core/Scripts/cumulate_time_2D.m
  # takes a quitte object, returns the present value time series.
  # the baseyear is the first year in the time series
  # option fixYear: From the discounted time series, substract the value in fixYear - defaults to none. In that case the value in the baseyear is zero  anyways by construction.

  ISOYear <- make.ISOyear()

  #Just do this for the specified variable, preserve all other columns.
  data = data[data$variable %in% c(nameVar,nameDisrate),]

  data$year = as.integer(as.character(data$period))
  if(length(levels(factor(data$year))) == 1){
   stop('This time series only contains one point - aggregation will not work!')
  }
  data=data[,!(names(data) == 'unit')]
  #convert to wide format
  data = reshape2::dcast(data,... ~ variable)
  #rename variable
  names(data)[names(data) == nameVar] = 'varToAggregate'

  if(nameDisrate %in% names(data) ){
    names(data)[names(data) == nameDisrate]  = 'disRate'
  }
  if(is.numeric(discount)){
   data$disRate = discount
  } else{
    warning('Endogenous interest discount is not validated yet.')
  }
  #group for all other columns:
  col_grp = names(data)[!(names(data) %in% c('varToAggregate','disRate','period','year'))]
  #calculate discount factor from discount rate:
  erg = data %>%
    group_by(!!!syms(col_grp)) %>%
    mutate(
      discFactor = cumprod((1 + !!sym('disRate'))^(-(!!sym('year') - lag(!!sym('year'),default=first(!!sym('year')),order_by=!!sym('year'))))),
      w = (!!sym('year') - first(!!sym('year'))) , # just for diagnostics
      discFactor2 = (1 + !!sym('disRate'))^(-(!!sym('year') - first(!!sym('year'))))    ## just for diagnostics this equals discFactor for time-indep disRate
  )

  #AJS question: how can I keep a column in the dataframe without grouping it?
  #this calculated annually compounded weight factors according to Elmar's method
  erg = erg %>%
    group_by(!!!syms(col_grp)) %>%
    mutate(
      weight1 = mapply(
        function(dt,dr) {
          sum( (1+dr)^(-seq(as.double(0.5),as.double(dt-0.5)) )
             * (1 - seq(as.double(0.5),as.double(dt-0.5))/dt)
              )
          },   # Why no use (1:dt) instead??
        (!!sym('year') - lag(!!sym('year'), default = first(!!sym('year')), order_by = !!sym('year'))),  # first element in year here doesnt matter anyways, will be thrown out later on..
        !!sym('disRate')
      ),
    weight2 = mapply(
      function(dt,dr) {
        sum( (1+dr)^(-(seq(as.double(0.5),as.double(dt-0.5)) - dt))
           * (seq(as.double(0.5),as.double(dt-0.5))/dt)
           )
         },
      (!!sym('year') - lag(!!sym('year'), default = first(!!sym('year')), order_by = !!sym('year'))),
      !!sym('disRate')
    ),
    weightSum = !!sym('weight2') + !!sym('weight1')  # just for diagnostics
    )


  yrs = as.integer(as.character(levels(factor(erg$year))))
  #yrs = yrs[yrs != min(yrs)]  ## all time steps but the first one.
  #calculate the whole discounted time series: FIXME how to do this more elegantly?
  erg_allT = do.call(rbind,lapply(yrs,function(p){
    tmp <- erg %>%
      filter(!!sym('year') <= p) %>%
      group_by(!!!syms(col_grp))  %>%
      summarise(
        discountedAggregate = sum(
          ( !!sym('varToAggregate') * !!sym('discFactor') * !!sym('weight2')
          + ( lag(!!sym('varToAggregate'), order_by = !!sym('year'))
            * lag(!!sym('discFactor'), order_by = !!sym('year')) * !!sym('weight1')
            )
          )[-1]       )
      ) %>% ungroup()
    tmp$period = p
    tmp
  }))

  names(erg_allT)[names(erg_allT)=='discountedAggregate'] = 'value'
  erg_allT$unit = NA
  erg_allT$variable = paste0(nameVar,'|aggregated')

  #shift resulting time series by the value in the year fixYear
  if(fixYear != 'none'){
#     if(! 'POSIXct' %in% class(fixYear)) fixYear = ISOYear(fixYear)
    erg_allT = erg_allT %>%
      group_by(!!!syms(col_grp)) %>%
      mutate(value = !!sym('value') - !!sym('value')[!!sym('period') == !!sym('fixYear')])
  }

  return(as.quitte(as.data.frame(erg_allT)))
}
