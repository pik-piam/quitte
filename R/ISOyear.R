#' speedily converting years to POSIXct values
#'
#' Converts integer years (e.g. `2023`) to [POSIXct] date/time values (e.g.
#' `2023-07-02 12:00:00 GMT`) corresponding to July 2, noon, which is the middle
#' of the (non-leap) year.  The function keeps a cache of already converted
#' values, as the underlying function [ISOdate()] is rather slow.
#'
#' @param year Vector of years to convert to [POSIXct].
#'
#' @return A vector of [POSIXct] values.
#'
#' @author Michaja Pehl
#'
#' @examples
#' ISOyear(c(2005, 2010, 2100, 1900))

#' @export
ISOyear <- (function() {
  cache <- new.env(hash = TRUE, parent = emptyenv())

  reset_cache <- function() {
    # initialise cache with default values
    assign('df',
           data.frame(
             year = as.integer(seq.int(1900, 2200, 5)),
             ISO = ISOdate(year = seq.int(1900, 2200, 5),
                           month = 7, day = 2)),
           envir = cache,
           inherits = FALSE)
  }

  add_year <- function(year) {
    assign('df',
           rbind(get(x = 'df', envir = cache, inherits = FALSE),
                 data.frame(year = year,
                            ISO = ISOdate(year = year, month = 7, day = 2))),
           envir = cache,
           inherits = TRUE)
  }

  reset_cache()

  return(function(year) {
    # any years missing from the cache?
    index <- get(x = 'df', envir = cache, inherits = FALSE) %>%
      getElement(name = 'year') %>%
      match(x = year) %>%
      is.na() %>%
      which()

    if (!is_empty(index)) {
      add_year(year[index])
    }

    get('df', envir = cache, inherits = FALSE) %>%
      `[`(get(x = 'df', envir = cache, inherits = FALSE) %>%
            getElement(name = 'year') %>%
            match(x = year),
          'ISO')
  })
})()
