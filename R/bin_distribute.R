#' Distribute into Equal Bins
#'
#' `bin_distribute(x, binsize)` distributes the items in `x` into the minimum
#' number of bins whose sizes differ at maximum by one and do not exceed
#' `binsize`.
#' `bin_distribute_sizes(count, binsize)` calculates the sizes of such bins for
#' `count` items.
#'
#' @md
#' @param x A character vector to be distributed into bins.
#' @param binsize The maximum bin size.
#' @param count The number of items to be binned.
#'
#' @return `bin_distribute()` returns a list with the sub-vectors of `x`.
#'   `bin_distribute_sizes()` returns a vector of sizes.
#'
#' @examples
#' regions <- c('CAZ', 'CHA', 'EUR', 'IND', 'JPN', 'LAM', 'MEA', 'NEU', 'OAS',
#'              'REF', 'SSA', 'USA', 'World')
#'
#' bin_distribute(regions, 5)
#' bin_distribute_sizes(length(regions), 5)
#'
#' bin_distribute(regions, 6)
#' bin_distribute_sizes(length(regions), 6)
#'
#' bin_distribute(regions, 7)
#' bin_distribute_sizes(length(regions), 7)
#'
#' @author Michaja Pehl

#' @export
bin_distribute <- function(x, binsize) {
    bins <- bin_distribute_sizes(length(x), binsize)

    lapply(seq_along(bins), function(i) {
        x[(cumsum(bins) - bins  + 1)[i]:cumsum(bins)[i]]
    })
}

#' @export
#' @rdname bin_distribute
bin_distribute_sizes <- function(count, binsize) {
    n_bins <- ceiling(count / binsize)
    bins <- c()
    for (b in 1:n_bins) {
        bins[b] <- ceiling(count / n_bins)
        count = count - bins[b]
        n_bins = n_bins - 1
    }
    return(bins)
}
