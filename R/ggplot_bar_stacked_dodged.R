#'
#' Stacked and dodged bar plot
#'
#' \code{ggplot_bar_stacked_dodged()} creates a bar plot with both stacked
#' (different categories atop each other) and dodged (different categories along
#' the x-axis) positioning. Useful for showing more dimensions.
#'
#' @param data a data frame
#' @param mapping \code{aes()} mapping with aesthetics \code{x}, \code{y},
#'                \code{fill} and \code{dodge}.
#' @return a \code{ggplot()} like object
#' @details The \code{fill} dimension will be stacked and the \code{dodge}
#'          dimension spread along the x-axis. Use further \code{ggplot2}
#'          functions like scales, themes and facets as usual. Automatically
#'          splits positive and negative values for plotting above and below
#'          zero bars.
#'
#' @section Example Plot:
#' \if{html}{\figure{ggplot_bar_stacked_dodged.png}{example plot}}
#'
#' @author Michaja Pehl
#' @examples
#' \dontrun{
#' require(dplyr)
#' require(ggplot2)
#'
#' set.seed(0)
#' (data <- crossing(x = c('left', 'center', 'right'),
#'          stack = c('bottom', 'middle', 'top'),
#'          category = LETTERS[26:23]) %>%
#'     mutate(value = abs(rnorm(n()))))
#'
#' ggplot_bar_stacked_dodged(data, aes(x = x, y = value, fill = stack,
#'                                     dodge = category)) +
#'     guides(fill = guide_legend(reverse = TRUE))
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#' @export
ggplot_bar_stacked_dodged <- function(data, mapping) {

    # get names from aesthetics mapping
    category <- as.character(rlang::get_expr(mapping$x))
    y        <- as.character(rlang::get_expr(mapping$y))
    fill     <- as.character(rlang::get_expr(mapping$fill))
    dodge    <- as.character(rlang::get_expr(mapping$dodge))

    # guardians
    if (is.null(category))
        stop("requires aesthetics mpping 'x'")
    if (is.null(y))
        stop("requires aesthetics mpping 'y'")
    if (is.null(fill))
        stop("requires aesthetics mpping 'fill'")
    if (is.null(dodge))
        stop("requires aesthetics mpping 'dodge'")
    if (!is.data.frame(data))
        stop("requires data frame")

    .levelsorunique <- function(x) {
        if (is.factor(x)) {
            return(levels(x))
        } else if (is.character(x)) {
            return(unique(x))
        } else {
            stop('only character vectors or factors')
        }
    }

    data[category] <- factor(data[[category]],
                               levels = .levelsorunique(data[[category]]))
    data[fill] <- factor(data[[fill]], levels = .levelsorunique(data[[fill]]))
    data[dodge] <- factor(data[[dodge]],
                          levels = .levelsorunique(data[[dodge]]))

    # generate temporary data frame in which 'x' and 'dodge' columns are
    # combined into one column which can be used on the x axis
    tmp <- data %>%
        select(setdiff(colnames(data), c(y, dodge))) %>%
        distinct() %>%
        # drop last entry, as there is no gap after the last bar
        filter(!!sym(category) != ifelse(is.factor(data[[category]]),
                                         last(levels(data[[category]])),
                                         last(data[[category]]))) %>%
        mutate(!!dodge := 'gap',
               !!y     := 0)

    # harmonise factor levels, so gaps are in the right place
    tmp[dodge]  <- factor(tmp[[dodge]],
                          levels = c(levels(data[[dodge]]), 'gap'))
    data[dodge] <- factor(data[[dodge]],
                          levels = c(.levelsorunique(data[[dodge]]), 'gap'))

    unite.name <- paste(category, dodge, sep = ".")

    # add temporary to plot data and arrange correctly
    tmp <- bind_rows(data, tmp) %>%
        unite(col = 'x.category', !!category, !!dodge, sep = '.',
              remove = FALSE) %>%
        arrange(!!!syms(c(category, dodge, fill)))

    unite.col <- which(colnames(tmp) == unite.name)

    tmp[unite.col] <- as.numeric(
        factor(getElement(tmp, unite.col),
               levels = unique(getElement(tmp, unite.col))))

    # information for x axis scale
    x.breaks <- seq(
        from       = (length(unique(getElement(data, dodge))) + 1) / 2,
        by         = length(unique(getElement(data, dodge))) + 1,
        length.out = length(unique(getElement(data, category))))

    labels.category <- as.character(unique(getElement(data, category)))

    ggplot(data = tmp, # pass data along for future use
           mapping = aes_string(x = unite.name, y = y, fill = fill)) +
        # plot positive and negative values individually
        geom_col() +
        scale_x_continuous(breaks = x.breaks, labels = labels.category)
}
