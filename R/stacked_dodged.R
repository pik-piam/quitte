#' Stacked and dodged bar plots
#'
#' Utility functions for plotting stacked (on top of each other) \emph{and}
#' dodged (next to each other) bars in the same figure.
#'
#' \code{add_stacked_dodged_xpos()} adds x-axis positions to a data frame for
#' plotting two categorical variables within a bar plot.
#' \code{calc_stacked_dodged_xlabels()} calculates matching label positions on
#' the x-axis.
#' \code{ggplot_bar_stacked_dodged()} uses both functions to generate a plot.
#'
#' @param data A data frame
#' @param ... A selection of two columns.  Both will be combined to form x-axis
#'     coordinates.  The first will form the outer iteration (groups), the
#'     second the inner iteration (bars within a group).  If unnamed, the
#'     column with calculated positions will be called \code{xpos}.
#' @param width The width of bars on the x-axis (default: \code{1}).
#' @param gap The width of the gap between bars (default: \code{1}).
#'
#' @return \code{add_stacked_dodged_xpos()} returns the input data frame with
#'     an additional column.  Row and column order are preserved.
#'     \code{calc_stacked_dodged_xlabels()} returns a named character vector
#'     for use with [ggplot2::scale_x_continuous()].
#'
#' @author Michaja Pehl
#'
#' @examples
#' require(tidyverse)
#'
#' set.seed(0)
#' (data <- crossing(a = factorise(c('left', 'center', 'right')),
#'                   b = factorise(c('top', 'middle', 'bottom')),
#'                   c = letters[1:4],
#'                   d = LETTERS[25:26]) %>%
#'         mutate(value = abs(rnorm(n())) + 0.2))
#'
#' ggplot(data = data %>%
#'            add_stacked_dodged_xpos(c('c', 'a'))) +
#'     geom_col(mapping = aes(x = xpos, y = value, fill = b)) +
#'     scale_x_continuous(
#'         breaks = calc_stacked_dodged_xlabels(data, c('c', 'a'))) +
#'     facet_wrap(~ d, ncol = 1, scales = 'free_x')
#'

#' @rdname stacked_dodged
#' @export
add_stacked_dodged_xpos <- function(data, ..., width = 1, gap = 1) {

    if (!is.data.frame(data))
        stop('requires a data.frame')

    .dots <- list(...)
    if (is.list(.dots[[1]]))
        .dots <- .dots[[1]]

    # ignore all but the first unspecified argument
    xpos <- .dots[1]

    xpos.column <- ifelse(is.null(names(xpos)), 'xpos', names(xpos))

    if (xpos.column %in% colnames(data))
        stop(ifelse(is.null(names(xpos)), 'Default c', 'C'),
             'olumn name "', xpos.column, '" already in use.')

    if (any(is.na(xpos[[1]][1:2])))
        stop('requires two columns to be combined')

    if (0 != length(missing.columns <- setdiff(xpos[[1]][1:2], colnames(data))))
        stop('column', ifelse(1 < length(missing.columns), 's ', ' '),
             paste(missing.columns, collapse = ', '), ' not found')

    group <- xpos[[1]][1]
    bar   <- xpos[[1]][2]

    # use the union of all elements within the <bar> column for the gap dummies
    # this way, there can't be any name clashes
    if (!is.factor(data[[bar]])) {
        bar.gap <- factor(paste(unique(data[[bar]]), collapse = ''),
                          levels = c(unique(data[[bar]]),
                                     paste(unique(data[[bar]]), collapse = '')))
    } else {
        # if the <bar> column is a factor, make sure the gap element is the
        # last level in the factor
        bar.gap <- paste(levels(data[[bar]]), collapse = '')
        bar.gap <- factor(bar.gap, levels = c(levels(data[[bar]]), bar.gap))
    }

    # generate all combinations of the <group> and <bar> columns, including
    # dummies for the gaps
    tmp <- crossing(!!sym(group) := factorise(unique_or_levels(data[[group]])),
                    !!sym(bar)   := factorise(levels(bar.gap))) %>%
        mutate(
            !!sym(xpos.column) :=
                # bars have width width, gaps have width gap
                ifelse(bar.gap != !!sym(bar), width, gap) %>%
                # sum everything up, subtract first bar, shift to position 1
                cumsum() - width + 1) %>%
    # remove gap, they were only needed to count out positions
    filter(!!sym(bar) != bar.gap) %>%
        droplevels()

    if (!is.factor(data[[group]]))
        tmp[[group]] <- as.character(tmp[[group]])
    if (!is.factor(data[[bar]]))
        tmp[[bar]] <- as.character(tmp[[bar]])

    left_join(data, tmp, c(group, bar)) %>%
        return()
}

#' @rdname stacked_dodged
#' @export
calc_stacked_dodged_xlabels <- function(data, ..., width = 1, gap = 1) {

    if (!is.data.frame(data))
        stop('requires a data.frame')

    # ignore all but the first unspecified argument
    xpos <- list(...)[1]

    if (any(is.na(xpos[[1]][1:2])))
        stop('requires two columns to be combined')

    n.groups <- length(unique_or_levels(data[[xpos[[1]][1]]]))
    n.bars   <- length(unique_or_levels(data[[xpos[[1]][2]]]))

    return(setNames(
        seq(from = (1 + (n.bars - 1) * width + 1) / 2,
            length.out = n.groups,
            by = n.bars * width + gap),
        unique_or_levels(data[[xpos[[1]][1]]])))
}
