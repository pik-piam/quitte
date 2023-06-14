#' Data Frame as List
#'
#' @param df A data frame.
#' @param names Index used for naming list items.  Integer or character, must
#'     work with `df[[names]]`.  Defaults to the first data frame column.
#' @param x Index used for list items.  Integer or character, must work with
#'     `df[[x]]`.  Defaults to the second data frame column.
#'
#' @return A list.
#'
#' @importFrom stats setNames
#'
#' @examples
#' (df <- data.frame(
#'     modules = c('power', 'macro', 'welfare', 'PE_FE_parameters',
#'                 'initialCap', 'aerosols'),
#'     `*` = c('IntC', 'singleSectorGr', 'utilitarian', 'iea2014', 'on',
#'             'exoGAINS'),
#'     check.names = FALSE
#' ))
#'
#' df.as.list(df, 'modules', '*')
#'
#' @export
df.as.list <- function(df, names = 1, x = 2)
{
  setNames(as.list(df[[x]]), df[[names]])
}
