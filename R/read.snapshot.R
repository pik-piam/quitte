#' Reads IAMC-style .csv files obtained as a IIASA snapshot into a quitte data frame,
#' filtering the data. This function is helpful if the csv file is large and R runs out
#' of memory loading it completely. This function requires head, tail and grep on your system.
#' If not supported, use read.quitte().
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

read.snapshot <- function(file, keep = list()) {
  unknowntype <- setdiff(names(keep), c("model", "scenario", "region", "variable", "unit", "period"))
  if (length(unknowntype) > 0) {
    stop("Unknown types to be kept: ", toString(unknowntype))
  }
  # temporary file
  tmpfile <- tempfile(pattern = "data", fileext = ".csv")
  if (length(setdiff(names(keep), "period")) > 0) {
    # always keep first lines of original file (comments, colnames), grep in the rest
    alwayskeep <- 20
    exitCode <- system(paste("head -n", alwayskeep, file, ">", tmpfile))
    if (exitCode != 0) stop("'head' command failed. Please use 'read.quitte()'")
    # the goal of the next lines is to grep one after the other through the elements of keep
    # keep = list(variable = "GDP|PPP", region = c("World", "FRA")) should get you
    # | grep -E '(GDP\|PPP)' | grep -E '(World|FRA)'
    # 1. escape | in variable names and do not grep for period
    cleanup <- function(x) {
      x <- gsub("[^A-Za-z0-9\\| ]", ".", x)
      x <- gsub("|", "\\|", x, fixed = TRUE)        
    }
    keepescaped <- lapply(keep[setdiff(names(keep), "period")], cleanup)
    # 2. collapse each element with a |
    keepcollapsed <- unlist(lapply(keepescaped, paste0, collapse = "|"))
    # generate a grep -E statement for each element of keep list
    greptext <- paste0(" | grep -E '(", keepcollapsed, ")'", collapse = "")
    command <- paste0("tail -n +", (alwayskeep + 1), " ", file, greptext, " >> ", tmpfile)
    exitCode <- system(command)
    if (exitCode != 0) stop("'tail' or 'grep' command created problems. Please use 'read.quitte()'")
  } else {
    file.copy(file, tmpfile, overwrite = TRUE)
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
