is_quitte <- function(x, verbose = FALSE) {
        required_columns <- list('model'    = c('character', 'factor'),
                                 'scenario' = c('character', 'factor'),
                                 'region'   = c('character', 'factor'),
                                 'variable' = c('character', 'factor'),
                                 'unit'     = c('character', 'factor'),
                                 'period'   = c('integer', 'POSIXct'),
                                 'value'    = 'numeric')

        is_quitte <- TRUE

        # check column existence ----
        if (any(missing_columns <- !names(required_columns) %in% colnames(x))) {
            if (isTRUE(verbose)) {
                message('Column', ifelse(sum(missing_columns) - 1, 's ', ' '),
                        paste(paste0('`',
                                     names(required_columns)[missing_columns],
                                     '`'),
                              collapse = ', '),
                        ifelse(sum(missing_columns) - 1, ' are ', ' is '),
                        'missing in `x`')
                is_quitte <- FALSE
            }
            else {
                return(FALSE)
            }
        }

        return(is_quitte)
}
