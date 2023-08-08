#' Allows to interactively filter data from quitte object
#'
#' @param data A quitte object or something that can be transformed into one by as.quitte
#' @param types vector of quitte columns for user to select data if more than one option available
#' @param keep list with quitte columns as names and data points that should always be kept.
#' If the column is not also in types, only the elements in that list are kept
#'
#' @examples
#' \dontrun{
#'   qe <- chooseFilter(quitte_example_dataAR6, types = c("model"),
#'                     keep = list(region = "World"))
#' }
#'
#' @author Oliver Richters
#'
#' @importFrom dplyr filter
#' @importFrom gms chooseFromList
#'
#' @export
chooseFilter <- function(data, types = c("model", "scenario", "region", "variable", "period"), keep = list()) {
  data <- droplevels(as.quitte(data, na.rm = TRUE))
  keep <- as.list(keep)
  stopifnot(all(types %in% names(data)))
  for (t in unique(c(types, names(keep)))) {
    userchoice <- NULL
    if (t %in% types) {
      items <- unique_or_levels(data[[t]])
      if (!all(keep[[t]] %in% items)) {
        warning("Data does not contain ", t, " ",
                paste(setdiff(keep[[t]], items), collapse = ", "), " although part of 'keep' vector")
      }
      choicelist <- setdiff(items, keep[[t]])
      if (length(choicelist) > 1 || (length(intersect(keep[[t]], items)) > 0 && length(choicelist) == 1)) {
        userinfo <- paste0("Choose ", t, "s",
                           if (length(keep[[t]] > 0)) paste0(" additional to ", paste0(keep[[t]], collapse = ", ")),
                           ". Press Enter for all ", t, "s.")
        userchoice <- chooseFromList(sort(choicelist), userinfo = userinfo, type = paste0(t, "s"))
        if (length(userchoice) == 0) userchoice <- choicelist
      } else {
        userchoice <- choicelist
      }
    }
    data <- droplevels(filter(data, .data[[t]] %in% c(keep[[t]], userchoice)))
  }
  return(data)
}
