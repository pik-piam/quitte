
#' Write .mif file
#'
#' A wrapper around \code{\link[readr]{write_lines}()} for writing files
#' conforming to the .mif standard.
#'
#' @param x A \code{\link{quitte}} data frame.
#' @param path Path or connection to write to.
#'
#' @author Michaja Pehl
#'
#' @examples
#' write.mif(quitte_example_data, tempfile())
#'
#' @importFrom dplyr mutate pull rename_with select
#' @importFrom tidyr spread unite
#' @importFrom tidyselect everything matches
#' @importFrom readr write_lines
#' @importFrom stringr str_to_title
#'
#' @export
write.mif <- function(x, path) {

    # guardians
    if (any(
        !is.data.frame(x),
        !all(c('model', 'scenario', 'region', 'variable', 'unit', 'period',
               'value') %in% tolower(names(x)))))
        stop('x must be a quitte data frame')

    # make column names upper-case
    x <- x %>%
        rename_with(.fn = str_to_title, .cols = matches('^[A-Za-z]+$')) %>%
        select('Model', 'Scenario', 'Region', 'Variable', 'Unit', 'Period',
               'Value') %>%
        spread(6, 7)

    paste(c(colnames(x), ''), collapse = ';') %>%
        write_lines(path, append = FALSE)

    x %>%
        unite(!!sym('text'), everything(), sep = ';') %>%
        mutate(!!sym('text') := paste0(!!sym('text'), ';')) %>%
        pull(!!sym('text')) %>%
        write_lines(path, append = TRUE)
}
