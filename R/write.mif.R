#' Write .mif file
#'
#' A wrapper around [`readr::write_lines] for writing files conforming to the
#' [`.mif` standard](https://redmine.pik-potsdam.de/projects/mo/wiki/Model_Intercomparison_File_Format_(mif)).
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
#'
#' @author Michaja Pehl
#'
#' @examples
#' write.mif(quitte_example_data, tempfile())
#'
#' @importFrom dplyr mutate pull rename_with select
#' @importFrom tidyr pivot_wider unite
#' @importFrom tidyselect everything matches
#' @importFrom readr write_lines
#' @importFrom stringr str_to_title

#' @export
write.mif <- function(x, path, comment_header = NULL, comment = '#',
                      append = FALSE) {

    default_columns <- c('Model', 'Scenario', 'Region', 'Variable', 'Unit',
                         'Period', 'Value')

    . <- NULL

    # guardians
    no_quitte <- any(
        !is.data.frame(x),
        !all(c('model', 'scenario', 'region', 'variable', 'unit', 'period',
               'value') %in% tolower(names(x))))
    if (no_quitte) {
        stop('x must be a quitte data frame')
    }

    # make column names upper-case
    x <- x %>%
        rename_with(.fn = str_to_title,
                    .cols = matches('^[A-Za-z][A-Za-z0-9_]*$')) %>%
        select(setdiff(default_columns, last(default_columns)),
               setdiff(colnames(.), default_columns),
               last(default_columns)) %>%
        arrange(.data$Period) %>%
        pivot_wider(names_from = 'Period', values_from = 'Value')

    # check existing header
    if (append) {
        # read the header, comment_header, and useless.last.column from file and
        # assign them to the environment
        header <- read_mif_header(path, ';', comment)$header

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
        paste(c(colnames(x), ''), collapse = ';') %>%
            write_lines(file = path,
                        append = ifelse(is.null(comment_header), FALSE, TRUE))
    }

    # write data
    x %>%
        unite(!!sym('text'), everything(), sep = ';') %>%
        mutate(!!sym('text') := paste0(!!sym('text'), ';')) %>%
        pull(!!sym('text')) %>%
        write_lines(file = path, append = TRUE)
}
