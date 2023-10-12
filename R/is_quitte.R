is_quitte <- function(x, verbose = FALSE) {
        required_columns <- list('model'    = c('character', 'factor'),
                                 'scenario' = c('character', 'factor'),
                                 'region'   = c('character', 'factor'),
                                 'variable' = c('character', 'factor'),
                                 'unit'     = c('character', 'factor'),
                                 'period'   = c('integer', 'POSIXct'),
                                 'value'    = 'numeric')
        return(TRUE)
}
