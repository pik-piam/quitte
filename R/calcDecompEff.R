#' Decomposes a change in a variable based on the changes of its factors
#' (Kaya-like)
#'
#' Computes decomposition for a change in time or policy of a variable. The
#' decomposition follows the methodology of the paper: "Some properties of an
#' exact energy decomposition model", Sun and Ang, 2000, Energy
#'
#' @param df a quitte object (with coluns model, scenario, region, variable,
#'   unit, period, value)
#' @param x a character vector detailing the explained variable from the
#'   decomposition as well as the factors in the decomposition. The explained
#'   variable should be named "explained" in the character vector.
#' @param bau the name of the reference scenario, as a character string. It is
#'   `NULL` as a default
#' @param pol the name of the policy scenario, as a character string. It is
#'   `NULL` as a default
#' @param gap either `"policy"` or `"time"`. If `"policy"`, `bau` and `pol`
#'   should be detailed. If `"time"`, `bau` and `pol` should stay `NULL`.
#'
#' @return A data frame with the effects of each component of the decomposition.
#'   The data frame contains new columns:
#'   - `explained` gives the name of the explained variable
#'   - `factors` gives the name of the factor considered (from the decomposition
#'     chain)
#'   - `type` gives the parameters:
#'     - `eff` is the result of the decomposition: how much of the change is to
#'       be attributed to the factor
#'     - `value` is the value of the factor
#'     - `lag` is the value of the factor in the reference scenario or in the
#'       previous period
#'     - `delta` is the difference in the factor's value between the policy and
#'       the reference, or between one period and another
#'
#' @examples
#' # In this example, emissions = ue * fe_ue * emi_fe
#' testdf = inline.data.frame(
#'   c("scenario ;period; variable; value",
#'
#'     "reference; 2015 ; emissions; 10",
#'     "reference; 2015 ; ue       ;  4",
#'     "reference; 2015 ; fe_ue    ;  2",
#'     "reference; 2015 ; emi_fe   ;  1.25",
#'
#'     "reference; 2050 ; emissions; 50",
#'     "reference; 2050 ; ue       ; 25",
#'     "reference; 2050 ; fe_ue    ;  1.25",
#'     "reference; 2050 ; emi_fe   ;  1.6",
#'
#'     "policy1; 2015 ; emissions; 10",
#'     "policy1; 2015 ; ue       ;  4",
#'     "policy1; 2015 ; fe_ue    ;  2",
#'     "policy1; 2015 ; emi_fe   ;  1.25",
#'
#'     "policy1; 2050 ; emissions; 20",
#'     "policy1; 2050 ; ue       ; 25",
#'     "policy1; 2050 ; fe_ue    ;  1.25",
#'     "policy1; 2050 ; emi_fe   ;  0.64",
#'
#'     "policy2; 2015 ; emissions; 10",
#'     "policy2; 2015 ; ue       ;  4",
#'     "policy2; 2015 ; fe_ue    ;  2",
#'     "policy2; 2015 ; emi_fe   ;  1.25",
#'
#'     "policy2; 2050 ; emissions; 10",
#'     "policy2; 2050 ; ue       ; 25",
#'     "policy2; 2050 ; fe_ue    ;  1.25",
#'     "policy2; 2050 ; emi_fe   ;  0.32")
#' )
#' testdf = as.quitte(testdf)
#' decomposition_chain = c(explained = "emissions", "ue","fe_ue","emi_fe")
#'
#' result = calcDecompEff(testdf,x = decomposition_chain,gap = "time")
#' result2 = calcDecompEff(testdf,x = decomposition_chain,bau = "reference",pol = "policy1")
#' result3 = calcDecompEff_scen(testdf, x = decomposition_chain, bau = "reference")
#'
#' @author Antoine Levesque
#'
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider separate
#'
#' @export

calcDecompEff <- function(df, x, bau=NULL, pol=NULL, gap = "policy"){

  #--- Initial Checks
  if (gap == "policy" & (is.null(bau) | is.null(pol))) stop("please provide bau and pol, if you want the differences in the policies")
  if (gap == "time" & !(is.null(bau) & is.null(pol))) warning("differentiating by time. Change gap to 'policy' if you want the differences in the policies")
  if (length(x[names(x) == "explained"]) != 1) stop("the explained variable has to be named 'explained in the x vector'")

  if (!(gap %in% c("time", "policy") )) stop("gap is either time or policy")

  #--- Internal Functions

  rewrite_form = function(form_r,gap_r,repl_str){
    if (gap_r == "time") form_r = gsub("bau","lag",form_r)
    form_r <- gsub("a_","X",form_r)
    form_r <- gsub(paste0(repl_str,"_"),"a_",form_r)
    form_r <- gsub("X",paste0(repl_str,"_"),form_r)
    form_r <- gsub("\n","",form_r)
  }

  levels2letters <- function(kaya_df,namesVar_in_str, explanatory_str){
    levels(kaya_df$variable)[levels(kaya_df$variable) %in% explanatory_str] = namesVar_in_str[ levels(kaya_df$variable)[levels(kaya_df$variable) %in% explanatory_str]]
    return(kaya_df)
  }

  letters2levels <- function(kaya_colnames,repl_str,namesVar_in_str){
    kaya_colnames <- gsub(paste0("^",repl_str,"(_.*)"),paste0(names(namesVar_in_str[which(namesVar_in_str == repl_str)]),"\\1"), kaya_colnames)
  }

  write_form0 = function(namesVar_in){

    #--- Internal Functions-----------

    suffix = function(txt,suf){
      paste(txt,suf,sep = "__")
    }

    select_combinations = function(data_combi, number_delta){
      result = data_combi[apply(data_combi,1,function(row) sum(grepl("delta",row)) == number_delta),]
      return(result)
    }
    dataFrame2string = function(df){
      df = apply(df,1,paste, collapse = " * ")
      df = paste(df, collapse = " + ")
      return(df)
    }

    addNumber2string = function(txt,num){
      txt = paste0("( 1/(1+",num,") ) * ( ",txt," ) ")
      return(txt)
    }

    createCombinations = function(df){
      data = expand.grid(df)
      data = character.data.frame(data)
    }

    #--- End of Internal Functions----------

    namesVar_in_minus_a = namesVar_in[namesVar_in != "a"]

    variables4Combi = as.data.frame(rbind(
      suffix(namesVar_in_minus_a,"delta"),
      suffix(namesVar_in_minus_a,"bau")))

    n = length(namesVar_in_minus_a)

    data = createCombinations(variables4Combi)
    list_strings = lapply(c(0:n), function(number){
      subsetCombi = select_combinations(data,number)
      stringCombi = dataFrame2string(subsetCombi)
      stringCombi = addNumber2string(stringCombi, number)
    })

    strings = paste(unlist(list_strings),collapse = "+")

    strings = paste0("a__eff = a__delta*(",strings,")")

    return(strings)
  }

  #---

  # Retain only the variables and scenarios of interest
  if( gap == "policy") {
    kaya <- df %>%
    filter_at(vars("scenario"), ~ . %in% c(bau, pol)) %>%
    filter_at(vars("variable"), ~ . %in% x)
  }
  if( gap == "time") {
    kaya <- df %>% filter_at(vars("variable"), ~ . %in% x)
  }

  explanatory = x[names(x) != "explained"]

  #replace variable names and scenario names by placeholders
  kaya$variable = factor(kaya$variable)
  namesVar_in = letters[1:length(explanatory)]
  names(namesVar_in) = explanatory

  kaya = levels2letters(kaya,namesVar_in,explanatory)

  if(gap == "policy"){
    kaya$scenario = factor(kaya$scenario)
    levels(kaya$scenario)[levels(kaya$scenario) == bau] = "bau"
    levels(kaya$scenario)[levels(kaya$scenario) == pol] = "pol"


    # prepare the dataset by computing the differences and spreading all variables

    kaya = kaya %>%
      pivot_wider(names_from = 'scenario',  values_from =  'value') %>%
      mutate(delta = .data$pol - .data$bau) %>%
      pivot_longer(cols = c("pol","bau","delta"), names_to  = "var", values_to = "value") %>%
      mutate(variable = paste(.data$variable, .data$var,sep = "__")) %>%
      select(-c("var","unit")) %>%
      pivot_wider(names_from = "variable", values_from = "value")
  }
  if(gap == "time"){
    .colgroup = setdiff(colnames(removeColNa(kaya)), c("unit","value","period"))
    kaya =kaya %>%
      select(-"unit") %>%
      group_by_at(.vars = vars(.colgroup)) %>%
      mutate(delta= c(NA,diff(.data$value)),
             lag = lag(.data$value, order_by = .data$period)) %>%
      ungroup() %>%
      pivot_longer(cols = c("value","delta","lag"), names_to = "var", values_to = "value" ) %>%
      mutate(variable = paste(.data$variable, .data$var, sep = "__")) %>%
      select(-"var") %>%
      pivot_wider(names_from ="variable", values_from = "value")


  }

  # declare the initial function depending upon the number of factors
  if (length(namesVar_in) == 1){
    form0 = "a__eff = a__delta"
  }else{
    form0 = write_form0(namesVar_in)
  }


  for (repl in namesVar_in){
    # for each variable, change the formula and compute the contribution to the delta of the explained variable.
    form = form0
    form = rewrite_form(form,gap,repl)
    kaya <- mutate_text(kaya, form)
  }

  # give the right names back
  for (repl in namesVar_in){

    colnames(kaya) <- letters2levels(colnames(kaya),repl,namesVar_in)

  }

  kaya = kaya %>% pivot_longer(names_to = "variable",values_to = "value",cols = matches("^(.*__delta)|(.*__eff)|(.*__value)|(.*__lag)|(.*__bau)|(.*__pol)$")) %>%
    separate(.data$variable,c("factors","type"), sep = "__")
  if (gap == "policy"){
    kaya = kaya %>% mutate(scenario = pol,
                                               type = ifelse(.data$type %in% c("bau"),
                                                             paste0(.data$type,"_",bau),
                                                             .data$type)
  )
  }
  kaya = kaya %>% mutate(explained = x["explained"])
  return(kaya)
}



#' @export
#' @rdname calcDecompEff
calcDecompEff_scen <- function(df, x, bau){

  scens = setdiff(getScenarios(df), bau)

  tmp = do.call(rbind,
                lapply(scens, function(scen){
                  tmp_kaya = calcDecompEff(df,x,bau = bau,pol = scen)
                }))
}
