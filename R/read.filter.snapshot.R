#' Reads IAMC-style .csv files obtained as a IIASA snapshot into a quitte data frame,
#' filtering the data. This function is helpful if the csv file is large and R runs out
#' of memory loading it completely.
#'
#' @md
#' @param file Path of single IAMC-style .csv/.mif file
#' @param keep list with quitte columns as names and data points that should be kept.
#'
#' @return A quitte data frame.
#'
#' @author Oliver Richters
#'
#' @examples
#' \dontrun{
#' read.filter.snapshot("snapshot.csv", list(scenario = c("CurPol", "NDC"), region = "World"))
#' }
#'
#' @importFrom dplyr filter
#'
#' @export

read.filter.snapshot <- function(file, keep = list()) {
  unknowntype <- setdiff(names(keep), c("model", "scenario", "region", "variable", "unit", "period"))
  if (length(unknowntype) > 0) {
    stop("Unknown types to be kept: ", toString(unknowntype))
  }
  # temporary file
  tmpfile <- tempfile(pattern = "data", fileext = ".csv")
  if (length(keep) > 0) {
    # always keep first lines of original file (comments, colnames)
    alwayskeep <- 20
    system(paste("head -n", alwayskeep, file, ">", tmpfile))
    # grep for everything that is being part of keep and append that to first lines
    greptext <- paste0("(,|;)(", paste0(unlist(keep), collapse = "|"), ")(,|;)")
    command <- paste0("tail -n +", (alwayskeep + 1), " ", file, " | grep -E '", greptext, "' >> ", tmpfile)
    system(command)
  } else {
    file.copy(file, tmpfile)
  }
  # read file and do correct filtering
  data <- read.quitte(tmpfile,
                      na.strings = c("UNDF", "NA", "N/A", "n_a", ""),
                      quote = '"',
                      drop.na = TRUE)
  unlink(tmpfile)
  for (t in names(keep)) {
    data <- droplevels(filter(data, .data[[t]] %in% keep[[t]]))
  }
  return(data)
}
