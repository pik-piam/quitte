#' @name add_remind_timesteps_columns
#'
#' @title Add Column Position and Width based on REMIND Timesteps
#'
#' @description Add columns \code{xpos} and \code{width} based on REMIND
#' timesteps for accurately plotting time-series bar plots from REMIND output.
#'
#' The warning "Ignoring unknown aesthetics: width" is due to a bug in
#' \code{ggplot2} and can savely be ignored.  The aesthetic is known and not
#' ignored.
#'
#' @param data A \code{data frame} or \code{quitte} object.
#' @param ... the column holding the period information; defaults to 'period'.
#' @param gaps Width of the gaps between columns; defaults to 0.
#'
#' @return A \code{data frame} or \code{quitte} object, same as input.
#'
#' @author Michaja Pehl
#'
#' @examples
#' library(dplyr)
#'
#' quitte_example_data %>%
#'     distinct(period) %>%
#'     add_remind_timesteps_columns() %>%
#'     as.data.frame()
#'
#' library(ggplot2)
#'
#' colours_PE <- c('Coal'       = '#0C0C0C',
#'                 'Oil'        = '#663A00',
#'                 'Gas'        = '#E5E5B2',
#'                 'Nuclear'    = '#FF33FF',
#'                 'Hydro'      = '#191999',
#'                 'Biomass'    = '#005900',
#'                 'Geothermal' = '#E51900',
#'                 'Wind'       = '#337FFF',
#'                 'Solar'      = '#FFCC00')
#'
#' ggplot() +
#'     geom_col(
#'         data = quitte_example_data %>%
#'             filter('r7552c_1p5C_Def-rem-5' == scenario,
#'                    'EUR' == region,
#'                    grepl('^PE\\|', variable)) %>%
#'             mutate(variable = sub('^PE\\|', '', variable)) %>%
#'             order.levels(variable = rev(names(colours_PE))) %>%
#'             add_remind_timesteps_columns(gaps = 0.2),
#'
#'         mapping = aes(x = xpos, width = width, y = value, fill = variable)) +
#'     scale_fill_manual(values = colours_PE,
#'                       name = NULL) +
#'     scale_x_continuous(breaks = unique(remind_timesteps$period),
#'                        name = NULL) +
#'     coord_cartesian(xlim = c(2005, 2100)) +
#'     labs(y = 'EJ/yr')
#'
#' @importFrom dplyr inner_join %>% group_by summarise ungroup

#' @export
add_remind_timesteps_columns <- function(data, ..., gaps = 0) {

    .period <- lazyeval::lazy_dots(...)
    if (length(.period)) {
        .period <- .period[[1]]$expr
    } else {
        .period <- 'period'
    }

    # guardians
    if (!is.data.frame(data))
        stop('Only works with data.frames')

    if (length(intersect(colnames(data), c('xpos', 'width'))))
        stop('Columns \'xpos\' and \'width\' can\'t be present already')

    if (0 > gaps)
        stop('Only works with gaps >= 0')

    inner_join(
        data,

        quitte::remind_timesteps %>%
            group_by(!!sym('period')) %>%
            summarise(width = sum(!!sym('weight') - !!sym('gaps')),
                      start = first(!!sym('year'))
                            + first(!!sym('weight')) %% 1
                            - 0.5 + !!sym('gaps') / 2,
                      end   = last(!!sym('year'))
                            - last(!!sym('weight')) %% 1
                            + 0.5 - !!sym('gaps') / 2,
                      xpos  = (!!sym('start') + !!sym('end')) / 2) %>%
            ungroup() %>%
            select('period', 'xpos', 'width'),

        setNames('period', .period)
    )
}


