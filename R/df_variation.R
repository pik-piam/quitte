#' Data Frame Variation
#'
#' Removes all columns from a data frame which have only identical data, to
#' facilitate a quick overview.
#'
#' @param x A data frame.
#'
#' @return A data frame.
#'
#' @examples
#' (quitte_example_data['Consumption' == quitte_example_data$variable,] -> x)
#'
#' df_variation(x)

#' @export
df_variation <- function(x) {
    for (i in rev(seq_len(ncol(x))))
        if (1 == length(unique(x[[i]])))
            x[[i]] <- NULL

    return(x)
}
