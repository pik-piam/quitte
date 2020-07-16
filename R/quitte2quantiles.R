#' calculate quantiles
#'
#' @param x dataframe to add quantiles
#' @param probs default=c(q0=0,q25=0.25,q50=0.5,q75=0.75,q100=1)
#' @param grouping default=c("region", "variable", "period", "scenario")
#' @author Gunnar Luderer, Lavinia Baumstark
#' @examples
#'
#'   \dontrun{
#'     p <- x.minmax = quitte2quantiles(x,probs=c("min"=0,"max"=1))
#'   }
#'
#' @export
quitte2quantiles <- function (x,probs=c(q0=0,q25=0.25,q50=0.5,q75=0.75,q100=1),
                              grouping=c("region","variable","period","scenario")  ){
   x %>%
        group_by(!!!syms(grouping)) %>%
        calc_quantiles(probs = probs ) %>%
        ungroup() %>%
        pivot_wider(names_from = 'quantile', values_from = 'value') ->
        x
    return(x)
}
