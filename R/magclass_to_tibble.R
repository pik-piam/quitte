#' Make a Tibble of a Magclass
#'
#' Sensible `magclass` to `tibble` conversion.
#'
#' @md
#' @param m A [`magpie`][magclass::magclass] object.
#'
#' @return A [`tibble`][tibble::tibble].
#'
#' @importFrom dplyr mutate select
#' @importFrom magclass as.data.frame getItems
#' @importFrom rlang sym syms
#' @importFrom tibble as_tibble
#'
#' @examples
#' magclass_to_tibble(magclass::maxample('pop'))

#' @export
magclass_to_tibble <- function(m) {
    if (!'magpie' %in% class(m)) {
        stop('m is not a magclass')
    }

    col_names <- c(unlist(strsplit(names(getItems(m)), '\\.')), 'value')

    m %>%
        as.data.frame() %>%
        as_tibble() %>%
        select(!!!syms(c('Region', 'Year',
                         paste0('Data', 1:(length(col_names) - 3)),
                         'Value'))) %>%
        `colnames<-`(col_names) %>%
        character.data.frame() %>%
        mutate(!!sym(col_names[2]) := as.integer(!!sym(col_names[2])))
}
