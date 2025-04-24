#' Write .mif file
#'
#' A wrapper around [`readr::write_lines`] for writing files conforming to the
#' [`.mif` standard](https://gitlab.pik-potsdam.de/rse/rsewiki/-/wikis/Model-Intercomparison-File-Format-(mif)).
#'
#'  `write.IAMCcsv()` uses commas as filed separators instead of semi-colons.
#'
#' @md
#' @param x A [`quitte`] data frame.
#' @param path Path or connection to write to.
#' @param comment_header Comment header to be written to the `.mif` file.
#'   Ignored if `append` is `TRUE`.
#' @param comment A character to prefix comment header lines with.  Must match
#'   existing comment characters in file `path` if `append` is `TRUE`.
#' @param append Overwrite existing files (`FALSE`, default), or append to them
#'   (`TRUE`).
#' @param sep Single character used to separate fields within a record.
#'  Defaults to `;`.
#' @param na String used for `NA` elements.  Defaults to `'NA'` for
#'   `write.mif()` and to `''` for `write.IAMCcsv()`.
#'
#' @author Michaja Pehl
#'
#' @examples
#' write.mif(quitte_example_data, tempfile())
#'
#' @importFrom dplyr across arrange mutate last pull rename_with select
#' @importFrom magrittr %>%
#' @importFrom readr write_lines
#' @importFrom stringr str_to_title
#' @importFrom tidyr pivot_wider replace_na unite
#' @importFrom tidyselect everything matches

#' @rdname write.mif
#' @export
write.mif <- function(x, path, comment_header = NULL, comment = '#',
                      append = FALSE, sep = ';', na = 'NA') {
    x <- as.quitte(x)
    if (nrow(x) == 0) warning("Writing empty data frame to ", path)

    # make column names upper-case
    x <- x %>% as.IAMCTimeseries()

    # check existing header
    if (append) {
        # read the header, comment_header, and useless.last.column from file and
        # assign them to the environment
        header <- read_mif_header(path, sep, comment)$header

        if (any(header != tolower(colnames(x)))) {
            stop('Column headers of x and path do not match.')
        }
    # or write a new one
    } else {
        # write comment header
        if (!is.null(comment_header)) {
            write_lines(x = paste('#', comment_header), file = path,
                        append = FALSE)
        }

        # write column header
        paste(c(colnames(x), ''), collapse = sep) %>%
            write_lines(file = path,
                        append = !is.null(comment_header))
    }

    # write data
    x %>%
        mutate(across(everything(), as.character)) %>%
        replace_na(replace = setNames(rep(list(na), ncol(x)), colnames(x))) %>%
        unite(!!sym('text'), everything(), sep = sep) %>%
        mutate(!!sym('text') := paste0(!!sym('text'), sep)) %>%
        pull(!!sym('text')) %>%
        write_lines(file = path, append = TRUE)
}

#' @rdname write.mif
#' @export
write.IAMCcsv <- function(x, path, comment_header = NULL, comment = '#',
                          append = FALSE, sep = ',', na = '') {
    write.mif(x, path, comment_header, comment, append, sep, na)
}
