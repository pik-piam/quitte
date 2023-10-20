#' Reads IAMC-style .csv files obtained as a IIASA snapshot into a quitte data frame,
#' filtering the data. This function is helpful if the csv file is large and R runs out
#' of memory loading it completely. This function requires head, tail and grep on your system.
#' If not supported, use read.quitte().
#'
#' @md
#' @param file Path of single IAMC-style .csv/.mif file
#' @param keep list with quitte columns as names and data points that should be kept. Using 'grep',
#' this list is used to extract the data before reading it into R. The more you restrict the data here,
#' the faster the data is read.
#' @param filter.function A function used to filter data during read, see read.quitte description.
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
#' @importFrom stats setNames
#'
#' @export

read.snapshot <- function(file, keep = list(), filter.function = NULL) {
  unknowntype <- setdiff(names(keep), c("model", "scenario", "region", "variable", "unit", "period"))
  if (length(unknowntype) > 0) {
    stop("Unknown types in 'keep': ", toString(unknowntype))
  }
  # join if multiple elements with same name exist in list
  joinelements <- function(v, list) return(setNames(list(unique(unname(unlist(list[names(list) == v])))), v))
  keep <- do.call(c, lapply(unique(names(keep)), joinelements, list = keep))

  # temporary file
  tmpfile <- tempfile(pattern = "data", fileext = ".csv")
  if (length(setdiff(names(keep), "period")) > 0) {
    # check whether system commands are supported
    testcommand <- c("grep", "head", "tail", "sed")
    exitcodes <- suppressWarnings(
        sapply(paste(testcommand, '--version'), system,
               ignore.stdout = TRUE, ignore.stderr = TRUE))
    if (any(0 != exitcodes)) {
        stop(paste(paste0('`', testcommand[0 != exitcodes], '`', collapse = ', '),
                  "are not available system commands, please use 'read.quitte'."))
    }
    # always keep first lines of original file (comments, colnames), grep in the rest
    alwayskeep <- 20
    system(paste("head -n", alwayskeep, file, ">", tmpfile))
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
    system(command)
  } else {
    file.copy(file, tmpfile, overwrite = TRUE)
  }
  joinedfilter <- function(data) {
    for (t in names(keep)) {
      data <- droplevels(filter(data, .data[[t]] %in% keep[[t]]))
    }
    if (is.function(filter.function)) {
      data <- filter.function(data)
    }
    return(data)
  }
  # read file and do correct filtering
  data <- read.quitte(tmpfile,
                      na.strings = c("UNDF", "NA", "N/A", "n_a", ""),
                      quote = '"',
                      drop.na = TRUE,
                      filter.function = joinedfilter)
  unlink(tmpfile)
  return(data)
}
