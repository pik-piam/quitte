#' Read .mif Header
#'
#' @md
#' @param file A path to a `.mif` file.
#' @param sep Column separator, defaults to ";".
#' @param comment A character which at line start signifies the optional comment
#'   header with metadata at the head of `file`.
#'
#' @return A `list` with elements `header`, `comment_header`, and
#'   `useless.last.column`.
#'
#' @author Michaja Pehl
#'
#' @importFrom dplyr first
#' @importFrom readr read_lines

read_mif_header <- function(file, sep, comment) {
    . <- NULL

    # Read as much of the file as is necessary to collect the comment header
    # and the column header
    n_max <- 1
    while (all(grepl('^#', header <- read_lines(file = file, n_max = n_max)))) {
        n_max = n_max * 2
    }

    # get the comment header
    comment_header <- header %>%
        grep(paste0('^', comment), ., value = TRUE) %>%
        sub(paste0('^', comment, '\\s*'), '', .)

    # get the column header
    header <- header %>%
        grep(paste0('^', comment), ., value = TRUE, invert = TRUE) %>%
        first()

    # Check if the last column of df is of class logical, i.e. if it was a
    # proper .mif file with the pointless trailing semi-colon.
    useless.last.column <- grepl(paste0(sep, '$'), header)

    # Convert column header into column names
    header <- header %>%
        strsplit(sep) %>%
        unlist() %>%
        tolower()

    return(list(header              = header,
                comment_header      = comment_header,
                useless.last.column = useless.last.column))
}