#' Read IAMC-style .csv files
#'
#' Reads IAMC-style .csv files into a quitte data frame.
#'
#' @param file Path of IAMC-style .csv file or vector of paths to read.
#' @param sep Column separator, defaults to ";".
#' @param quote Quote characters, empty by default.
#' @param na.strings Entries to interpret as NA; defaults to
#'        \code{c("UNDF", "NA", "N/A", "n_a")}
#' @param convert.periods If \code{TRUE}, periods are converted to POSIXct. If
#'        \code{FALSE} (the default), periods are numerical.
#' @param check.duplicates If \code{TRUE} a duplicates check will be performed
#'        on the data. For time- and memory-critical applications this can be
#'        switched off.
#' @return A quitte data frame.
#'
#' @author Michaja Pehl
#'
#' @examples
#' \dontrun{
#' read.quitte(c("some/data/file.mif", "some/other/data/file.mif"))
#' read.quitte("some/data/file.csv", sep = ",", quote = '"')
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import utils
#'
#' @export
read.quitte <- function(file,
                        sep = ";",
                        quote = "",
                        na.strings = c("UNDF", "NA", "N/A", "n_a"),
                        convert.periods = FALSE,
                        check.duplicates = TRUE) {

    if (!length(file))
        stop('\'file\' is empty.')

    # read quitte for all supplied files
    quitte <- tibble()
    for (f in file) {
        quitte <- bind_rows(
            quitte,
            .read.quitte(f, sep, quote, na.strings, convert.periods)
        )
    }

    quitte <- factor.data.frame(quitte)

    # check for duplicate entries, ignoring values
    if (check.duplicates && quitte %>% select_('-value') %>% anyDuplicated())
        warning('Duplicates in resulting quitte')

    # apply quitte "class" attribute
    class(quitte) <- c(class(quitte), 'quitte')

    return(quitte)
}

.read.quitte <- function(file,
                        sep = ";",
                        quote = "",
                        na.strings = c("UNDF", "NA", "N/A", "n_a"),
                        convert.periods = FALSE) {

    # Check the header for correct names, periods all in one block and no
    # additional columns after the periods
    header <- read.table(file, header = TRUE, sep = sep, quote = quote,
                         na.strings = na.strings, nrows = 1,
                         check.names = FALSE, strip.white = TRUE) %>%
        colnames() %>%
        tolower()

    default.columns  <- c("model", "scenario", "region", "variable", "unit")
    # FIXME: relax to handle other than 4-digit periods
    period.columns   <- grep("^[0-9]{4}$", header)


    # Check if the last column of df is of class logical, i.e. if it was a
    # propper .mif file with the pointless trailing semi-colon.
    useless.last.column <- last(header) == ""

    if (useless.last.column)
        header <- header[-length(header)]

    if (!all(header[1:5] == default.columns))
        stop("missing default columns in header of file ", file)

    if (last(period.columns) != length(header))
        stop("unallowed extra columns in header of file ", file)

    if (!all(seq_range(range(period.columns)) == period.columns))
        stop("period columns not all in one block in header of file ", file)

    periods <- header[period.columns]

    colClasses <- c(rep("character", period.columns[1] - 1),
                    rep("numeric",   length(period.columns)))
    if (useless.last.column)
        colClasses <- c(colClasses, "NULL")

    # read actual data
    data <- read.table(file, header = TRUE, sep = sep, quote = quote,
                       na.strings = na.strings, colClasses = colClasses,
                       check.names = FALSE, strip.white = TRUE) %>%
        tbl_df()

    colnames(data) <- tolower(colnames(data))

    # convert to long format
    data <- data %>%
        gather_("period", "value", periods)

    # convert periods
    if (convert.periods) {
        ISOyear <- make.ISOyear(seq(2005, 2150, by = 5))
        data$period <- ISOyear(data$period)
    } else {
        data$period <- as.integer(as.character(data$period))
    }

    return(tbl_df(data))
}

