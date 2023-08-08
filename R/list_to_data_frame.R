#' Convert mapping list to data frame
#'
#' @param l A named list of character vectors.
#' @param ... Unquoted names of category and item columns. Defaults to
#'        'category' and 'item'.
#' @param category Name of category column. Defaults to 'category'.
#' @param item Name of item column. Defaults to 'item'.
#'
#' @return A data frame.
#'
#' @author Michaja Pehl
#'
#' @examples
#' l <- list(Africa = c('Egypt', 'Tanzania'),
#'           Europe = c('Portugal', 'Ukraine', 'Denmark'))
#' list_to_data_frame(l, region, country)
#'
#' @importFrom lazyeval lazy_dots
#' @importFrom tibble tibble_

#' @export
list_to_data_frame <- function(l, ...) {
    dots <- lazy_dots(...)

    if (length(dots) < 1) {
        category = 'category'
    } else {
        category = as.character(dots[[1]]$expr)
    }

    if (length(dots) < 2) {
        item = 'item'
    } else {
        item = as.character(dots[[2]]$expr)
    }

    list_to_data_frame_(l, category, item)
}

#' @export
#' @rdname list_to_data_frame
list_to_data_frame_ <- function(l, category = 'category', item = 'item') {
    d <- tibble()
    for (name in names(l))
        d <- bind_rows(
            d,
            tibble(!!sym(category) := name,
                   !!sym(item)     := l[[name]])
        )
    return(d)
}
