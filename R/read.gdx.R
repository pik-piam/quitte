#' Read item from `.gdx` file as quitte data frame
#'
#' `read.gdx()` is a wrapper function for [`gamstransfer::readGDX()`] that
#' returns a quitte data frame.
#'
#' @param gdxName Path to a `.gdx` file.
#' @param requestList.name Name of the item to read.
#' @param fields Fields to read from variables and equations.  Any of `l`, `m`,
#'     `lo`, `up`, `s` (or the long forms `level`, `marginal`, `lower`, `upper`,
#'     and `scale`).  `all` will return all fields.  Ignored when reading sets or
#'     parameters.
#' @param colNames String vector of column names to override dimension and field
#'     names.
#' @param factors Deprecated.  Do not use any more.
#' @param squeeze If `TRUE` (the default), squeeze out any zero or EPS stored in
#'        the GDX container.
#'
#' @return A quitte data frame.
#' @author Michaja Pehl
#'
#' @importFrom cli cli_abort
#' @importFrom lifecycle deprecated deprecate_warn is_present
#' @importFrom tibble as_tibble
#'
#' @export
read.gdx <- function(gdxName, requestList.name, fields = "l", colNames = NULL,
                     factors = deprecated(), squeeze = TRUE) {
    if (is_present(factors)) {
        deprecate_warn('0.3135.0', 'quitte::read.gdx(factors = )',
                       details = 'Please do not use the argument anymore.')
    }

    # functions ----
    is.Alias <- function(d) {
        'Alias' == d[['class']]
    }

    is.Set <- function(d) {
        'Set' == d[['class']]
    }

    is.Parameter <- function(d) {
        'Parameter' == d[['class']]
    }

    is.Scalar <- function(d) {
        0 == d[['dimension']]
    }

    is.integer.string <- function(x) {
        all(grepl('^[0-9]+$', x))
    }

    nothing.defined <- function(d) {
        is.null(d[['records']])
    }

    make.gamstransfer.names <- function(s) {
        for (i in seq_along(s)) {
            matches <- s[i] == s
            count <- sum(matches)
            if (1 < count) {
                s[matches] <- paste(s[i], seq_len(count), sep = '_')
            }
        }
        return(s)
    }

    convert_field_names <- function(fields) {
        # convert short (gdxrrw) to long (gamstransfer) field names, check for
        # unknown field names
        field_codes <- c('l'  = 'level',
                         'm'  = 'marginal',
                         'lo' = 'lower',
                         'up' = 'upper',
                         's'  = 'scale')

        if ('all' %in% fields) {
            return(unname(field_codes))
        }

        known_fields <- c(field_codes, names(field_codes), 'all')
        unknown_fields <- !fields %in% known_fields
        if (any(unknown_fields)) {
            cli_abort(c(
                paste('{sum(unknown_fields)} unknown field{?/s}:',
                      '{paste0("`", fields[unknown_fields], "`")}'),
                'i' = 'Use any of {paste0("`", known_fields, "`")} instead'))
        }

        return(unique(na.omit(c(setdiff(fields, names(field_codes)),
                                field_codes[fields]))))
    }

    # load data ----
    d <- gamstransfer::readGDX(loadFrom = gdxName,
                               symbols = requestList.name)[[1]]

    ## reload aliases ----
    if (is.Alias(d)) {
        d <- gamstransfer::readGDX(loadFrom = gdxName,
                                   symbols = d[['aliasWith']])[[1]]
    }

    # select correct fields ----
    # only equations and variables have fields, parameters always report value
    fields <- if (is.Set(d)) {
        character(0)
    } else if (is.Parameter(d)) {
        'value'
    } else {
        convert_field_names(fields)
    }

    # select correct columns ----
    column_selector <- c(make.gamstransfer.names(d[['domain']]), fields)
    if (!is.null(colNames)) {
        if (length(colNames) != length(column_selector)) {
            cli_abort(c(
                paste('Length of `colNames` ({length(colNames)}) does not',
                      'match number of selected columns',
                      '({length(column_selector)}).'),
                'i' = 'colNames: {paste0("`", colNames, "`")}',
                'i' = 'selected columns: {paste0("`", column_selector, "`")}'))
        }

        column_selector <- setNames(column_selector, colNames)
    } else {
        column_selector <- setNames(
            column_selector,
            c(  # unique names for identical defining sets
                make.names(d[['domain']], unique = TRUE),
                # always return `level` as `value`
                sub('level', 'value', fields, fixed = TRUE)))
    }

    # filter data ----
    result <- if (nothing.defined(d)) {
        matrix(nrow = 0, ncol = length(column_selector),
               dimnames = list(NULL, column_selector)) %>%
            as_tibble() %>%
            mutate(across(all_of(d[['domain']]), as.character),
                   across(all_of(fields), as.numeric))
    } else {
        d[['records']] %>%
            as_tibble() %>%
            select(all_of(column_selector)) %>%
            mutate(across(where(is.factor), as.character),
                   across(where(is.integer.string), as.numeric))
    }

    # squeeze out stored zeros / EPS ----
    # gamstransfer::readGDX always returns stored zeros (and reads EPS back as 0),
    # whereas gdxrrw::rgdx(squeeze = TRUE) drops them. Replicate this for compatibility
    # so the `squeeze` argument behaves identically: drop records whose primary value/level
    # is 0. Special vals like NA and +/-Inf are kept
    if (squeeze && length(fields) > 0 && !is.Scalar(d) && nrow(result) > 0) {
        value_col <- names(result)[[ncol(result) - length(fields) + 1]]
        v <- result[[value_col]]
        result <- result[is.na(v) | v != 0, , drop = FALSE]
    }

    # extract scalars ----
    if (is.Scalar(d)) {
        result <- setNames(result[[1]], requestList.name)
    }

    return(result)
}
