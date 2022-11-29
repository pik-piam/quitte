#' Populate Data With Sequence Along Range
#'
#' Generate sequences of n equidistant data points for a column in a data frame.
#'
#' @md
#' @param df A data frame.
#' @param column <[tidy-select][dplyr_tidy_select]> The column with data to
#'     populate.
#' @param n Length of the sequence to return.  Defaults to 100.
#'
#' @return A data frame.
#'
#' @examples
#' require(dplyr, warn.conflicts = FALSE, quietly = TRUE)
#' require(tidyr, warn.conflicts = FALSE, quietly = TRUE)
#'
#' tibble(A = (1:3) ^ 2,
#'        B = exp(0:2)) %>%
#'     pivot_longer(everything()) %>%
#'     arrange(name, value) %>%
#'     print() %>%
#'     df_populate_range(value, n = 6)
#'
#' @importFrom dplyr group_by filter mutate pull select slice_head ungroup
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom rlang !! !!! := .data sym syms
#' @importFrom tibble tibble
#' @importFrom tidyr nest pivot_longer unnest
#' @importFrom tidyselect all_of
#'
#' @export
df_populate_range <- function(df, column, n = 100) {
    # extract column names from tidyselect
    column <- df %>%
        slice_head() %>%
        select({{ column }}) %>%
        colnames()

    if (1 < length(column))
        stop('Can operate only on a single column, got ', length(tmp_column),
             ': ', paste(tmp_column, collapse = ', '))

    if (1 > length(column))
        stop('No column to operate on: ', {{ column }})

    tmp_column <- paste(colnames(df), collapse = '')

    df %>%
        # group by everything except the data column
        group_by(!!!syms(setdiff(colnames(df), column))) %>%
        # select minimum and maximum of data column
        filter(.data[[column]] %in% range(.data[[column]])) %>%
        # process each range individually
        nest(!!sym(tmp_column) := all_of(column)) %>%
        mutate(!!sym(column) := map(
            .data[[tmp_column]],
            # create a range from minimum to maximum of length n
            function(x, n) {
                tibble(!!sym(column) := seq_range(range = x %>%
                                                     pull(!!sym(column)) %>%
                                                     range(),
                                                 length.out = n))
            },
            n = n)) %>%
        # clean up
        select(-all_of(tmp_column)) %>%
        unnest(.data[[column]]) %>%
        ungroup()
}
