#' Recreate a data frame as a tribble
#'
#' Prints a representation of `df` using [`tribble()`][tibble::tribble] for
#' constructing examples or configuration data.
#'
#' @param df A data frame.
#'
#' @examples
#' quitte_example_data |>
#'     head() |>
#'     print() |>
#'     tribbelise()

#' @export
tribbelise <- function(df)
{
    x <- lapply(
        seq_len(ncol(df)),
        function(i)
        {
            x <- paste0(
                if (i == 1) rep.int('    ', nrow(df)),   # indent first column
                c(paste0('~', colnames(df)[[i]]),
                  if (is.numeric(df[[i]])) {
                      format(df[[i]])                    # justified numbers
                  } else {
                      paste0("'", df[[i]], "'")
                  }),
                # All but the last column are followed by a comma and separating
                # whitespace.  The last column and row is followed by a closing
                # brace.
                c(rep.int(ifelse(i < ncol(df), ',   ', ','), nrow(df)),
                  ifelse(i < ncol(df), ',', ')')))

            # justify columns
            x_max <- max(sapply(x, nchar))

            if (i < ncol(df)) {
                x <- sapply(x,
                            function(x)
                            {
                                paste(c(x, rep.int(' ', x_max - nchar(x))),
                                      collapse = '')
                            },
                            USE.NAMES = FALSE)
            }

            return(x)
        })

    c('tribble(',
      sapply(seq_len(lengths(x)[[1]]),
             \(i) paste(sapply(x, getElement, name = i), collapse = ''))) |>
        cat(sep = '\n')
}
