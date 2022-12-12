#' Read IAMC-style .csv or .xlsx files
#'
#' Reads IAMC-style .csv or .xlsx files into a quitte data frame.
#'
#' @md
#' @param file Path of IAMC-style .csv or xlsx. file or vector of paths to read.
#' @param sep Column separator, defaults to ";".
#' @param quote Quote characters, empty by default.
#' @param na.strings Entries to interpret as NA; defaults to
#'     `c("UNDF", "NA", "N/A", "n_a")`
#' @param convert.periods If `TRUE`, periods are converted to
#'     [`POSIXct`][base::DateTimeClasses].  If `FALSE` (the default), periods
#'     are numerical.
#' @param check.duplicates If `TRUE` a duplicates check will be performed on the
#'     data.  For time- and memory-critical applications this can be switched
#'     off.
#' @param factors Return columns as factors (`TRUE`, the default) or not.
#' @param drop.na Should `NA` values be dropped from the `quitte`?
#' @param comment A character which at line start signifies the optional comment
#'   header with metadata at the head of `file`.  The comment header, if
#'   present, is returned as a `comment_header` attribute.  If multiple files
#'   are read, the `comment_header` attribute is a list of comment headers with
#'   file paths as names.
#'
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
#' @importFrom rlang .data is_empty
#' @importFrom readr problems read_delim read_lines
#' @importFrom readxl read_excel excel_sheets
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

    .read.quitte <- function(f, sep, quote, na.strings, convert.periods,
                             drop.na, comment) {

        if (grepl("\\.xlsx?$", f)) {
            data <- read_excel(path = f, sheet = if ('data' %in% excel_sheets(f)) 'data' else 1)
            periods <- names(data)[grep("^[0-9]{4}$", names(data))]
            data <- as.quitte(pivot_longer(data, all_of(periods), names_to = 'period', values_drop_na = drop.na))
            return(data)
        }
        # Check the header for correct names, periods all in one block and no
        # additional columns after the periods

        # read header ----
        # read the header, comment_header, and useless.last.column from f and
        # assign them to the environment
        foo <- read_mif_header(f, sep, comment)
        header              <- foo$header
        comment_header      <- foo$comment_header
        useless.last.column <- foo$useless.last.column

        default.columns  <- c("model", "scenario", "region", "variable", "unit")
        # FIXME: relax to handle other than 4-digit periods
        period.columns <- grep("^[0-9]{4}$", header)

        if (!all(header[1:5] == default.columns))
            stop("missing default columns in header of file ", f)

        if (last(period.columns) != length(header))
            stop("unallowed extra columns in header of file ", f)

        if (!all(seq_range(range(period.columns)) == period.columns))
            stop("period columns not all in one block in header of file ", f)

        periods <- header[period.columns]

        colClasses <- paste(c(rep('c', period.columns[1] - 1),
                              rep('n',   length(period.columns)),
                              ifelse(useless.last.column, '-', '')),
                            collapse = '')

        # read data ----
        data <- suppressWarnings(
            read_delim(file = f, quote = quote, col_names = c(header),
                       col_types = colClasses, delim = sep, na = na.strings,
                       skip = length(comment_header) + 1,
                       comment = comment)
        )

        # catch any parsing problems
        data_problems <- if (nrow(problems(data))) {
            problems(data)
        }

        data <- data %>%
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

        # re-attach parsing problems
        attr(data, 'problems') <- data_problems
        attr(data, 'comment_header') <- comment_header

        return(data)
    }

    # read quitte for all supplied files ----
    quitte <- tibble()
    quitte_problems <- tibble()
    comment_header <- list()
    for (f in file) {
        data <- .read.quitte(f, sep, quote, na.strings, convert.periods,
                             drop.na, comment)
        quitte <- bind_rows(quitte, data)
        quitte_problems <- bind_rows(quitte_problems, attr(data, 'problems'))
        comment_header <- c(comment_header,
                            setNames(list(attr(data, 'comment_header')), f))
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

    # pass parsing problems along and issue warning
    attr(quitte, 'problems') <- if (nrow(quitte_problems)) {
        quitte_problems
    }

    if (nrow(quitte_problems)) {
        if (interactive()) {
            warning('One or more parsing issues, call `readr::problems()` for ',
                    'details.',
                    call. = FALSE)
        } else {
            warning.length <- getOption('warning.length')
            options(warning.length = 8170)
            warning('One or more parsing issues:\n',
                    quitte_problems %>%
                        as.data.frame() %>%
                        format() %>%
                        capture.output() %>%
                        paste(collapse = '\n'))
            options(warning.length = warning.length)
        }
    }

    # pass comment_header along as attribute, if any
    if (!is_empty(unlist(comment_header))) {
        if (1 == length(file)) {
            comment_header <- unlist(comment_header, use.names = FALSE)
        }
        attr(quitte, 'comment_header') <- comment_header
    }

    return(quitte)
}
