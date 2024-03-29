
#' Duplicate rows
#'
#' Duplicate rows in a data frame, modifying a specified column.
#'
#' @param data A `data frame` or `quitte` object.
#' @param ...,column A key-value pair of the column to modify.
#'
#' @return A `data frame` or `quitte` object, same as input.
#'
#' @examples
#' require(dplyr)
#' (data <- tibble(region   = rep(c('AFR', 'CHN'), 2),
#'                     variable = paste('Var', c(1, 1, 2, 2)),
#'                     value    = 1:4))
#'
#' data %>% duplicate(region = 'World')

#' @export
duplicate <- function(data, ...) {

    column <- list(...)[1]

    duplicate_(data, column)
}

#' @export
#' @rdname duplicate
duplicate_ <- function(data, column) {

    # guardians
    if (!is.data.frame(data))
        stop('Only works with data frames.')

    if (!is.list(column) || is.null(names(column)))
        stop('Need a key-value list for column to modify.')

    if (!names(column[1]) %in% colnames(data))
        stop('No column \'', names(column[1]), '\' found in data.')

    overwrite(
        data,

        data %>%
            mutate(!!sym(names(column[1])) := column[[1]]),

        except = NA
    )
}
