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
#' @param drop.na Should NA values be dropped from the `quitte`?
#' @param comment A character which at line start signifies the optional comment
#'   header with metadata at the head of `file`.
#'
#' @md
#' @param factors Return columns as factors (`TRUE`) or not.
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
#' @importFrom dplyr as_tibble bind_rows distinct first last tibble
#' @importFrom forcats as_factor
#' @importFrom rlang .data
#' @importFrom readr read_delim read_lines
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom utils read.table
#'
#' @export
read.quitte <- function(file,
                        sep = ";",
                        quote = "",
                        na.strings = c("UNDF", "NA", "N/A", "n_a"),
                        convert.periods = FALSE,
                        check.duplicates = TRUE,
                        factors = TRUE,
                        drop.na = FALSE,
                        comment = '#') {

    if (!length(file))
        stop('\'file\' is empty.')

    .read.quitte <- function(file, sep, quote, na.strings, convert.periods,
                             drop.na, comment) {

        . <- NULL

        # Check the header for correct names, periods all in one block and no
        # additional columns after the periods

        # read header ----
        # Read as much of the file as is necessary to collect the comment header
        # and the column header
        n_max <- 1
        while (all(grepl('^#',
                         header <- read_lines(file = file, n_max = n_max)))) {
            n_max = n_max * 2
        }

        ## get the comment header ----
        comment_header <- header %>%
            grep(paste0('^', comment), ., value = TRUE) %>%
            sub(paste0('^', comment, '\\s*'), '', .)

        ## get the column header ----
        header <- header %>%
            grep(paste0('^', comment), ., value = TRUE, invert = TRUE) %>%
            first()

        # Check if the last column of df is of class logical, i.e. if it was a
        # propper .mif file with the pointless trailing semi-colon.
        useless.last.column <- grepl(paste0(sep, '$'), header)

        # Convert column header into column names
        header <- header %>%
            strsplit(sep) %>%
            unlist() %>%
            tolower()

        default.columns  <- c("model", "scenario", "region", "variable", "unit")
        # FIXME: relax to handle other than 4-digit periods
        period.columns <- grep("^[0-9]{4}$", header)


        if (!all(header[1:5] == default.columns))
            stop("missing default columns in header of file ", file)

        if (last(period.columns) != length(header))
            stop("unallowed extra columns in header of file ", file)

        if (!all(seq_range(range(period.columns)) == period.columns))
            stop("period columns not all in one block in header of file ", file)

        periods <- header[period.columns]

        colClasses <- paste(c(rep('c', period.columns[1] - 1),
                              rep('n',   length(period.columns)),
                              ifelse(useless.last.column, '-', '')),
                            collapse = '')

        # read data ----
        data <- read_delim(file = file, quote = quote, col_names = c(header),
                           col_types = colClasses, delim = sep, na = na.strings,
                           skip = length(comment_header) + 1,
                           comment = comment) %>%
            # convert to long format
            pivot_longer(all_of(periods), names_to = 'period',
                         values_drop_na = drop.na)

        # convert periods ----
        if (convert.periods) {
            ISOyear <- make.ISOyear(seq(2005, 2150, by = 5))
            data$period <- ISOyear(data$period)
        } else {
            data$period <- as.integer(as.character(data$period))
        }

        return(data)
    }

    # read quitte for all supplied files ----
    quitte <- tibble()
    for (f in file) {
        quitte <- bind_rows(
            quitte,
            .read.quitte(f, sep, quote, na.strings, convert.periods, drop.na,
                         comment)
        )
    }

    if (factors) {
        quitte <- quitte %>%
            # preserve order of scenarios for order of .mif files
            mutate(scenario = as_factor(.data$scenario)) %>%
            factor.data.frame()
    }

    # check for duplicate entries, ignoring values
    if (check.duplicates) {
        distinct_rows <- quitte %>%
            select(-'value') %>%
            distinct() %>%
            nrow()
        if (nrow(quitte) != distinct_rows)
            warning('Duplicates in resulting quitte')
    }

    # apply quitte "class" attribute
    class(quitte) <- c('quitte', class(quitte))

    return(quitte)
}
