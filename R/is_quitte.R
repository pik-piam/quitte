is_quitte <- function(x, verbose = FALSE) {
        required_columns <- list('model'    = c('character', 'factor'),
                                 'scenario' = c('character', 'factor'),
                                 'region'   = c('character', 'factor'),
                                 'variable' = c('character', 'factor'),
                                 'unit'     = c('character', 'factor'),
                                 'period'   = c('integer', 'POSIXct'),
                                 'value'    = 'numeric')

        .is_quitte <- TRUE

        # check column existence ----
        missing_columns <- !names(required_columns) %in% colnames(x)
        if (any(missing_columns)) {
            if (isTRUE(verbose)) {
                message(
                    'Column', ifelse(sum(missing_columns) - 1, 's ', ' '),
                    paste(paste0('`',
                                 names(required_columns)[missing_columns],
                                 '`'),
                          collapse = ', '),
                    ifelse(sum(missing_columns) > 1, ' are ', ' is '),
                    'missing in `x`')

                .is_quitte <- FALSE
            }
            else {
                return(FALSE)
            }
        }

        # check column classes ----
        column_classes <- x %>%
            select(any_of(names(required_columns))) %>%
            sapply(class)

        wrong_column_classes <- sapply(
            seq_along(column_classes),
            function(i) {
                !any(column_classes[[i]] %in%
                    required_columns[[names(column_classes)[[i]]]])
            }) %>%
            setNames(column_classes)

        if (any(wrong_column_classes)) {
            if (isTRUE(verbose)) {
                message(
                    'Wrong column class',
                    ifelse(sum(wrong_column_classes) > 1, 'es', ''), ':\n',
                    paste(
                        paste0(
                            '  ',
                            names(column_classes[which(wrong_column_classes)]),
                            ' [', column_classes[which(wrong_column_classes)],
                            '], not ',
                            required_columns[
                                names(column_classes[wrong_column_classes])]
                        ),
                        collapse = '\n'))

                .is_quitte <- FALSE
            }
            else {
                return(FALSE)
            }
        }

        if (isTRUE(verbose)) {
            return(invisible(.is_quitte))
        }
        else {
            return(.is_quitte)
        }
}
