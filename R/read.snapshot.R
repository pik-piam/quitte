#' Reads IAMC-style .csv or .xlsx files obtained as a IIASA snapshot into a quitte data frame,
#' allowing to filter the loaded data. If head, tail and grep are on your system, a pre-filtering
#' improves performance.
#'
#' @md
#' @param file Path of single IAMC-style .csv/.mif file
#' @param keep list with quitte columns as names and data points that should be kept.
#' If head, tail and grep are available, this list is used to extract the data before reading it
#' into R. The more you restrict the data here, the faster the data is read.
#' @param filter.function A function used to filter data during read, see read.quitte description.
#' This allows for more complex filtering, but no performance-enhancing pre-filtering using grep is used.
#' The 'keep' list and the 'filter.function' can be combined.
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

read.snapshot <- function(file, keep = list(), filter.function = identity) {
  unknowntype <- setdiff(names(keep), c("model", "scenario", "region", "variable", "unit", "period"))
  if (length(unknowntype) > 0) {
    stop("Unknown types in 'keep': ", toString(unknowntype))
  }
  # join if multiple elements with same name exist in list
  joinelements <- function(v, list) return(setNames(list(unique(unname(unlist(list[names(list) == v])))), v))
  keep <- do.call(c, lapply(unique(names(keep)), joinelements, list = keep))

  if (!file.exists(file)) stop("file '", file, "' not found.")

  # temporary file
  tmpfile <- tempfile(pattern = "data", fileext = gsub("^.*\\.", ".", basename(file)))
  if (length(setdiff(names(keep), "period")) > 0 && !grepl("\\.xlsx?$", file)) {
    # check whether system commands are supported
    testcommand <- c("grep", "head", "tail")
    notavailable <- Sys.which(testcommand) == ""
    if (any(notavailable)) {
        message(paste0("`", testcommand[notavailable], "`", collapse = ", "),
                " are not available system commands, so the entire file is read.")
    } else {
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
    }
  }
  if (!file.exists(tmpfile)) { # if either system commands do not exist or something went wrong
    file.copy(file, tmpfile, overwrite = TRUE)
  }
  joinedfilter <- function(data) {
    for (t in names(keep)) {
      data <- droplevels(filter(data, .data[[t]] %in% keep[[t]]))
    }
    data <- filter.function(data)
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
