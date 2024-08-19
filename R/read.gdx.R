#' Read item from `.gdx` file as quitte data frame
#'
#' `read.gdx()` is a wrapper function for either [`gdxrrw::rgdx()`] or
#' [`gamstransfer::readGDX()`] that returns a quitte data frame.
#'
#' `read.gdx()` will use [`gdxrrw::rgdx()`] if [gdxrrw](gdxrrw-package) is
#' installed and the option `quitte_force_gamstransfer` is not `TRUE`, otherwise
#' it will use [`gamstransfer::readGDX()`].
#'
#' @param gdxName Path to a `.gdx` file.
#' @param requestList.name Name of the item to read.
#' @param fields Fields to read from variables and equations.  When using
#'     [gdxrrw](gdxrrw-package), any of `l`, `m`, `lo`, `up`, `s`.  When using
#'     using [gamstransfer](gamstransfer-package), `level`, `marginal`, `lower`,
#'     `upper`, and `scale` are understood as well.  `all` will return all
#'     fields.  Ignored when reading sets or parameters.
#' @param colNames String vector of column names to override dimension and field
#'     names.
#' @param factors Deprecated.  Do not use any more.
#' @param squeeze If `TRUE`, squeeze out any zero or EPS stored in the GDX
#'        container.  Ignored when using [gamstransfer](gamstransfer-package).
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

    # Check if gdxrrw package is installed
    no_gdx_package <- !any(c('gdxrrw', 'gamstransfer') %in%
                               .packages(all.available = TRUE))
    if (no_gdx_package) {
        stop('Neither `gdxrrw` nor `gamstransfer` package is installed.\n',
             'See https://www.gams.com/latest/docs/API_R_GAMSTRANSFER.html')
    }

    # use gdxrrw unless gamstransfer is forced or gdxrrw is not installed
    use_gdxrrw <- !any(getOption('quitte_force_gamstransfer', default = FALSE),
                       !'gdxrrw' %in% .packages(all.available = TRUE))

    if (use_gdxrrw) {
        .read.gdx_gdxrrw(gdxName, requestList.name, fields, colNames, squeeze)
    }
    else {
        .read.gdx_gamstransfer(gdxName, requestList.name, fields, colNames,
                               squeeze)
    }
}

init_gdxrrw <- function() {
    if (!(done <- gdxrrw::igdx(silent = TRUE, returnStr = FALSE))) {
        if ("Windows" == getElement(Sys.info(), "sysname")) {
            path <- strsplit(Sys.getenv("PATH"), ";")[[1]]
            path <- grep("gams", path, value = TRUE, ignore.case = TRUE)
            path <- grep("%", path, value = TRUE, invert = TRUE)

            for (p in path)
                if (done <- gdxrrw::igdx(p, silent = TRUE))
                    break
        }
        else {
            suppressWarnings(
                s <- system("which gams | xargs dirname", intern = TRUE,
                            ignore.stderr = TRUE))
            if (0 != length(s)) {
                done <- gdxrrw::igdx(s, silent = TRUE)
            }
            else {
                done <- FALSE
            }
        }
    }
    return(done)
}

.read.gdx_gdxrrw <- function(gdxName, requestList.name, fields, colNames,
                             squeeze) {
    if (!init_gdxrrw()) {
        stop("Could not load gdx libraries")
    }

    gdxName <- path.expand(gdxName)

    # if reading variable or equation, read specific fields (e.g. "lo", "m")
    info <- gdxrrw::gdxInfo(gdxName, dump = FALSE, returnList = TRUE)
    read.fields <- tolower(requestList.name) %in% tolower(c(info$variables,
                                                            info$equations))

    # read the first (or only) field
    if (read.fields) {
        requestList <- list(name = requestList.name, field = fields[[1]])
        item <- gdxrrw::rgdx(gdxName, requestList, squeeze = squeeze)
    }
    else {
        requestList <- list(name = requestList.name)
        item <- gdxrrw::rgdx(gdxName, requestList, squeeze = squeeze)
    }

    # if item is a scalar, return a named vector
    if (0 == item$dim) {
        data <- as.vector(item$val)
        names(data) <- requestList.name
        return(data)
    }

    # convert dimension info
    data <- list()
    for (d in 1:(item$dim)) {
        val <- item$val[,d]
        uel <- item$uels[[d]]

        if (all(grepl("^[0-9]+$", uel)))
            uel <- as.numeric(uel)

        data[[d]] <- c(uel[val])
    }

    # add first (or only) field
    if (dim(item$val)[[2]] > d) {
        d <- d + 1
        data[[d]] <- c(item$val[,d])
    }

    # read additional fields
    if (read.fields) {
        for (field in fields[-1]) {
            d <- d + 1
            requestList <- list(name = requestList.name, field = field)
            item <- gdxrrw::rgdx(gdxName, requestList, squeeze = squeeze)
            data[[d]] <- c(item$val[,item$dim + 1])
        }
    }

    if (read.fields) {
        field.names <- sub("^l$", "value", fields)
    }
    else {
        field.names <- "value"
    }

    if (is.null(colNames)) {
        if (length(data) > length(item$domains)) {
            names(data) <- c(item$domains, field.names)
        } else {
            names(data) <- c(item$domains)
        }
    }
    else {
        names(data) <- colNames
    }

    data <- as_tibble(data.frame(data))

    return(data)
}

.read.gdx_gamstransfer <- function(gdxName, requestList.name, fields, colNames,
                                   squeeze) {
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
    if (is.Set(d)) {
        fields <- character(0)
    } else if (is.Parameter(d)) {
        fields <- 'value'
    } else {
        fields <- convert_field_names(fields)
    }

    # select correct columns ----
    column_selector <- c(colnames(d[['records']])[seq_len(d[['dimension']])],
                         fields)
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
    result <- as_tibble(d[['records']]) %>%
        select(all_of(column_selector)) %>%
        mutate(across(where(is.factor), as.character))

    # extract scalars ----
    if (is.Scalar(d)) {
        result <- setNames(result[[1]], requestList.name)
    }

    return(result)
}
