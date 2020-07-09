#' Aggregates or disaggregates a data frame
#'
#' Aggregates or disaggregates the values of a data frame according to a mapping
#'
#' By default \code{"weights"} is set to \code{NULL}. For aggregations, this means that values will
#' be summed as they are. For disaggregations, each component of the larger category will
#' take the same value as the larger category, or \code{"scaleWeights"} is \code{TRUE},
#' each component will be given an even weight. For aggregations, \code{"weights"} can also be the
#' name of a variable contained in \code{"data"}. \code{"weights"} may also be a data frame
#'
#'
#' @param data a data frame.
#' @param mapping a data frame connecting the resolution in \code{"data"}
#'                and the wished resolution
#' @param by (named) vector giving the correspondence between the column name of \code{"data"}
#'           and of the \code{"mapping"}
#' @param subset2agg subset of variables for which the (dis)aggregation is applied
#'        . If \code{NULL} (the default), the (dis)aggregation is applied to
#'        all variables.
#' @param only.new If \code{FALSE} (the default), add the (dis)aggregated data frame to existing
#'                 ones. If \code{TRUE}, return only the (dis)aggregated data frame.
#' @param na.rm If \code{TRUE} (the default), remove items calculated as
#'              \code{NA}.
#' @param weights a data frame, a variable name as a character vector, or \code{NULL}.
#'        See details
#' @param forceAggregation binary. If \code{TRUE}, (dis)aggregation will be applied
#'        even though the items contained contained in the data and in the data do not fully match.
#'        The data is reduced to the items covered both by the mapping and the data.
#' @param autodetect this parameter takes the values auto, aggregate or disaggregate. If auto (the default)
#'                   the function tries to auto-detect whether this is an aggregation or a disaggregation.
#'                   If aggregate, it will aggregate, if disaggregate, it will disaggregate.
#' @param scaleWeights binary. If \code{TRUE}, weights are scaled so that the sum of the
#' components equals the value of the larger category.
#' @param variable Column name of variables. Defaults to \code{"variable"}.
#' @param value Column name of values. Defaults to \code{"value"}.
#' @param unit Column name of units. Defaults to \code{"unit"}.
#' @param weight_val_col name of the value column in the \code{"weigths"} data frame,
#'         if \code{"weigths"} is a data frame
#' @param weight_item_col name of the item column in the \code{"weigths"} data frame,
#'         if \code{"weigths"} is a data frame. The item column is the column corresponding to the mapping
#' @param fun aggregation function to use.  Defaults to \code{sum}.
#'
#' @return A data frame.
#'
#' @examples
#' library(tidyr)
#' library(dplyr)
#' data <- inline.data.frame(c(
#'   "model;    scenario;   region;   variable;           unit;         period;   value",
#'   "REMIND;   Baseline;   USA;      GDP per Capita|MER; US$2005/yr;   2010;     40000",
#'   "REMIND;   Baseline;   USA;      GDP per Capita|MER; US$2005/yr;   2020;     50000",
#'   "REMIND;   Baseline;   USA;      Population;         million;      2010;     300",
#'   "REMIND;   Baseline;   USA;      Population;         million;      2020;     350",
#'   "REMIND;   Baseline;   CHN;      GDP per Capita|MER; US$2005/yr;   2010;     7000",
#'   "REMIND;   Baseline;   CHN;      GDP per Capita|MER; US$2005/yr;   2020;     8000",
#'   "REMIND;   Baseline;   CHN;      Population;         million;      2010;     1300",
#'   "REMIND;   Baseline;   CHN;      Population;         million;      2020;     1400"))
#'
#' mapping = inline.data.frame(c(
#'   "region;      New_region",
#'   "USA;         GLO",
#'   "CHN;         GLO"
#' ))
#'
#' mapping2 = inline.data.frame(c(
#'   "Item      ;         Item_new",
#'   "Population;         Urban Population ",
#'   "Population;         Rural Population"
#' ))
#'
#' weights = inline.data.frame(c(
#'   "region; itemI           ;   weight",
#'   "USA   ; Urban Population;      0.5",
#'   "USA   ; Rural Population;      0.2",
#'   "CHN   ; Urban Population;      2",
#'   "CHN   ; Rural Population;      1"
#' ))
#'
#' #Regional Aggregation
#' aggregate_map(data,mapping, by = "region", subset2agg = c("Population"))
#'
#' #Regional Weighted Aggregation
#' aggregate_map(data,mapping, by = "region", subset2agg = "GDP per Capita|MER",
#'               weights = "Population")
#'
#' #Variable Weigthed Disaggregation
#' aggregate_map(data,mapping2, by = c("variable" = "Item"),
#'               subset2agg = c("Population"),weights = weights,
#'               weight_val_col = "weight", weight_item_col = "itemI")
#'
#'
#' @author Antoine Levesque
#'
#' @importFrom stats setNames
#' @export




aggregate_map <- function(data,
                          mapping,
                          by,
                          subset2agg = NULL,
                          only.new = TRUE,
                          na.rm = TRUE,
                          weights = NULL,
                          forceAggregation = F,
                          autodetect = "auto",
                          scaleWeights = T,
                          variable = "variable",
                          value = "value",
                          unit = "unit",
                          weight_val_col = "weight_val_col",
                          weight_item_col = NULL,
                          fun = sum){


  .colnames = colnames(data)
  .mapnames = colnames(mapping)
  .bynames = names(by)
  if (is.null(.bynames))  .bynamesleft = by else .bynamesleft = unname(c(.bynames[.bynames != ""], by[.bynames == ""]))
  .mapnamesright = setdiff(.mapnames, by)

  #------- Internal Function -----------------------

  scale_weights = function(weight_data,mapping_df, DetailedColumn,.by , value_col,na.rm_val){

    if(identical(DetailedColumn,.by)){
      DetailedColumn_map = DetailedColumn
      DetailedColumn_df  = DetailedColumn
    } else{
      DetailedColumn_map = unname(.by[DetailedColumn])
      DetailedColumn_df = DetailedColumn
    }
        .mapCoarseCol = setdiff(colnames(mapping_df), DetailedColumn_map)
    .cols = setdiff(c(colnames(weight_data),.mapCoarseCol), c(value_col,DetailedColumn_df) )

    res = weight_data %>%
      left_join(mapping_df, by = .by) %>%
      group_by_(.dots = .cols) %>%
      mutate_(.dots = setNames(list(lazyeval::interp(~value_col/sum(value_col, na.rm = na.rm_val),
                                       value_col = as.name(value_col),
                                       na.rm_val = na.rm_val)),
                               value_col)) %>%
      ungroup()

      res[.mapCoarseCol] = NULL
      return(res)
  }

  #------- Guardians -----------------------

  if (!variable %in% .colnames)    stop("No column '", variable, "' found'")

  if (!value %in% .colnames)    stop("No column '", value, "' found'")

  if (!unit %in% .colnames)    stop("No column '", unit, "' found'")
  if (weight_val_col %in% .colnames)    stop("'",weight_val_col, "' in the columns of the data, please chose another name for 'weight_val_col'")

  if (!all(.bynamesleft %in% .colnames)) stop(paste(.bynamesleft, collapse = " "), " are not all in colnames(data)")

  if (length(by) > 1) stop("currently, the function only disaggregates one column at a time")

  if (length(setdiff(.mapnames, by)) > length(by) ) stop("there are more remaining columns in the mapping than the length of 'by'")

  if (length(setdiff(.mapnames, by)) < length(by) ) stop("there are less remaining columns in the mapping than the length of 'by'")

  if (!all(by  %in% .mapnames) ) stop(paste(by, collapse = " "), " are not all in colnames(mapping)")

  if (is.data.frame(weights)){
    if(!(weight_val_col %in% colnames(weights)))    stop("No column '", weight_val_col, "' found in the weights'")
    if(!(weight_item_col %in% colnames(weights)))    stop("No column '", weight_item_col, "' found in the weights'")
    weight_item_col
  }

  if (.mapnamesright %in% .bynamesleft) stop("The mapping column name '",.mapnamesright,"' equals the data colum name '",.bynamesleft,"'. Please remove that ambiguity")

  if (! autodetect %in% c("auto","disaggregate","aggregate")) stop("autodetect must take 'auto', 'disaggregate', 'aggregate")
  #-------- Determine whether there is a "unit" column------------------

  if (unit %in% .colnames){
    UnitInData <- T
  }else{
    stop("No column '", unit, "' found.")
  }

  #-------- Determine whether this is an aggregation or a disaggregation -------
  if(autodetect == "auto"){
    aggregation = length(getColValues(mapping,by))  >  length(getColValues(mapping,.mapnamesright))
  } else if (autodetect == "aggregate"){
    aggregation = T
  } else if (autodetect == "disaggregate"){
    aggregation = F
  }

  .nameDetailedColumn = ifelse(aggregation,.bynamesleft,.mapnamesright)


  weights_character =  is.vector(weights) & length(weights) == 1
  #-------- Filter for variables used on rhs ------------------------------
  #Ajouter weights si c'est character.
  if (!is.null(subset2agg)){
    .data <- data %>%
    filter_(.dots = lazyeval::interp(~variable %in% subset2agg,
                                     variable = as.name(variable),
                                     subset2agg = subset2agg))
  } else{
    .data <- data
  }


  #-------- Are the regions/variables in the data correctly covered by the mapping? --------------

  items_map = getColValues(mapping,by)
  items_data = getColValues(.data, .bynamesleft)


  diff_map_data = setdiff(items_map, items_data)
  diff_data_map = setdiff(items_data, items_map)

  if(length(diff_map_data) > 0 | length(diff_data_map) >0 ){

    message_mismatch = paste0("the number of regions/variables does not correspond: \n",
                              "these regions/variables are in the mapping but not in data : ",paste(diff_map_data, collapse = " "), "\n",
                              "these regions/variables are in data but not in the mapping : ", paste(diff_data_map, collapse = " "),"\n",
                              " Reduce to the same dimension")

    if (!forceAggregation) {

      stop(message_mismatch)}

    else {

      warning(message_mismatch)
      inter_data_map = intersect(items_map, items_data)

      .data = .data %>%  filter_(.dots = lazyeval::interp(~.bynamesleft %in% inter_data_map,
                                                          .bynamesleft = as.name(.bynamesleft),
                                                          inter_data_map = inter_data_map))
    }

  }

  #---- Computation of the weights according to each case -----
  if (aggregation){

    if( is.null(weights)){

      .weights_df = data.frame(getColValues(mapping,by), 1)
      colnames(.weights_df) <- c(.nameDetailedColumn, weight_val_col)

    } else if ( weights_character){

      #considers the character vector points to a variable in the data frame
      .weights_df <- data %>%
        filter_(.dots = lazyeval::interp(~variable %in% weights,
                                         variable = as.name(variable),
                                         weights = weights))
      names(.weights_df)[names(.weights_df) == value] = weight_val_col

      .weights_df = .weights_df %>% select_(.dots = lazyeval::interp(~-variable, variable = as.name(variable)))

      if (UnitInData) .weights_df = .weights_df %>% select_(.dots = lazyeval::interp(~-unit, unit = as.name(unit)))

      if (scaleWeights) .weights_df <- scale_weights(.weights_df,mapping,.nameDetailedColumn, by,weight_val_col,na.rm)


    } else if (is.data.frame(weights)){
      #does not scale the weights if this is a df
      .weights_df <- weights
      if (scaleWeights) .weights_df <- scale_weights(.weights_df,mapping,.nameDetailedColumn, by,weight_val_col,na.rm)

    } else {
      stop("class of weights is not supported for aggregation")
    }
  }else{
    if (is.null(weights)){
      .weights_df = data.frame(getColValues(mapping,.nameDetailedColumn), 1)
      colnames(.weights_df) <- c(.nameDetailedColumn, weight_val_col)

      if (scaleWeights) .weights_df <- scale_weights(.weights_df,mapping, .nameDetailedColumn,.nameDetailedColumn,weight_val_col,na.rm)

    } else if (is.data.frame(weights)){
      .weights_df <- weights

      names(.weights_df)[names(.weights_df) == weight_item_col] = .nameDetailedColumn

      if (scaleWeights) .weights_df <- scale_weights(.weights_df,mapping, .nameDetailedColumn,.nameDetailedColumn,weight_val_col,na.rm)
    } else { stop("For disaggregation, only NULL or a data.frame is supported for weights")}
  }


  if (any(.weights_df[[weight_val_col]] == "Inf")) ("Some elements of the weighting matrix are Inf. Infinite weights are not allowed!")

  #---- Multiply the values by the weights, even if the weights = 1 ----
  .colGroups_weight = intersect(c(.colnames,.mapnames), colnames(.weights_df))

  .data = .data %>% left_join(mapping, by = by) %>%
    left_join(.weights_df, by = .colGroups_weight )   %>%  #.nameDetailedColumn
    mutate_(.dots = setNames( list(lazyeval::interp(~value * weight_val_col,
                                               value = as.name(value),
                                               weight_val_col = as.name(weight_val_col)))
                              ,value))
  .data[weight_val_col] = NULL
  #---- For aggregations, sum over the more detailed column
  #---- For disaggregations, delete the less detailed column and rename the more detailed one.
  if(aggregation){

    .colGroups = setdiff(colnames(.data), c(.bynamesleft,value,weight_val_col))

    .data = .data %>%
    group_by_(.dots = .colGroups) %>%
    summarise_( .dots = setNames( list(lazyeval::interp(~fun(value, na.rm = na.rm),
                                           value = as.name(value),
                                           na.rm = na.rm))
                          ,value)) %>%
    ungroup()

    } else {

      .data[.bynamesleft] = NULL

    }

    names(.data)[names(.data) == .mapnamesright] = .bynamesleft

    if(!only.new ) .data = rbind(data, .data)

return(.data)
}

