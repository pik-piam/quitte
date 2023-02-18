#' Class "quitte" ~~~
#'
#' The quitte class is a more standardized data.frame format. `is.quitte`
#' tests if `x` is an quitte-object, `as.quitte` transforms `x`
#' to an quitte-object (if possible).
#'
#'
#' @name quitte-class
#' @aliases quitte-class as.quitte as.quitte-methods
#' as.quitte,character-method
#' @docType class
#' @param x An object that should be either tested or transformed as/to an
#' quitte-object.
#' @param periodClass integer or POSIXct
#' @param addNA modifies a factor by turning NA into an extra level (so that NA
#' values are counted in tables, for instance).
#' @param na.rm if set to TRUE entries with value NA will be removed
#' @author Jan Philipp Dietrich
#' @keywords classes
#' @importFrom reshape2 melt
#' @importFrom forcats fct_na_value_to_level
#' @importFrom tibble as_tibble
#'
#' @export
as.quitte <- function(x, periodClass = "integer", addNA = FALSE, na.rm = FALSE) { # nolint
    UseMethod("as.quitte", x)
}

#' @export
as.quitte.character <- function(x, periodClass = "integer", addNA = FALSE, na.rm = FALSE) { # nolint
    if (all(file.exists(x) & grepl("\\.(mif|csv|xlsx)$", x)))
        return(as.quitte(read.quitte(x), periodClass =
                                  periodClass, addNA = addNA, na.rm = na.rm))
    stop(
        "Provided character cannot be converted to quitte as it does not seem to be a valid file path!"
    )
}

#' @method as.quitte quitte
#' @export
as.quitte.quitte <- function(x, periodClass = "integer", addNA = FALSE, na.rm = FALSE) { # nolint
    if (is.quitte(x, warn = FALSE)) {
        if (addNA) x <- qaddNA(x)
        if (na.rm) x <- x[!is.na(x$value), ]
        return(x)
    } else {
        class(x) <- class(x)[class(x) != "quitte"]
        return(as.quitte.data.frame(x, periodClass = periodClass, addNA = addNA, na.rm = na.rm))
    }
}

#' @method as.quitte data.frame
#' @export
as.quitte.data.frame <- function(x, periodClass = "integer", addNA = FALSE, na.rm = FALSE) { # nolint
    if (!(periodClass %in% c("integer", "POSIXct")))
        stop("periodClass must be in c('integer', 'POSIXct')")
    storeAttributes <- attributes(x)[-match(c("names", "row.names", "class"),
                                             names(attributes(x)))]
    mandatoryColumns <-
        c("model",
          "scenario",
          "region",
          "variable",
          "unit",
          "period",
          "value")
    factorColumns <- c("model", "scenario", "region", "variable", "unit")
    colnames(x) <- tolower(colnames(x))
    colnames(x)[colnames(x) == "year"] <- "period"
    colnames(x)[colnames(x) == paste0("data", 1)] <- "scenario"
    colnames(x)[colnames(x) == paste0("data", 2)] <- "model"
    colnames(x)[colnames(x) == paste0("data", 3)] <- "variable"

    if (!("value" %in% colnames(x)) & any(!is.na(suppressWarnings(as.integer(colnames(x)))))) {
      x <- suppressMessages(melt(x))
      colnames(x)[which(colnames(x) == "value") - 1] <- "period"
    }

    if (!all(mandatoryColumns %in% colnames(x))) {
        if (!("model"    %in% colnames(x)))
            x <- cbind(x, model = fct_na_value_to_level(factor(NA),
                                                        level = '(Missing)'))
        if (!("scenario" %in% colnames(x)))
            x <- cbind(x, scenario = fct_na_value_to_level(factor(NA),
                                                           level = '(Missing)'))
        if (!("region"   %in% colnames(x)))
            x <- cbind(x, region = as.factor("GLO"))
        if (!("variable" %in% colnames(x)))
            x <- cbind(x, variable = fct_na_value_to_level(factor(NA),
                                                           level = '(Missing)'))
        if (!("unit"     %in% colnames(x)))
            x <- cbind(x, unit = fct_na_value_to_level(factor(NA),
                                                       level = '(Missing)'))
        if (periodClass == "POSIXct")
            if (!("period"   %in% colnames(x)))
                x <- cbind(x, period = as.POSIXct(NA))
        if (periodClass == "integer")
            if (!("period" %in% colnames(x)))
                  x <- cbind(x, period = as.integer(NA))
        if (!("value"    %in% colnames(x)))
              stop("Data frame cannot be converted. A column \"value\" has to be provided!")
    }
    factorCheck <- sapply(x[, factorColumns], is.factor) # nolint
    if (!all(factorCheck)) {
        for (i in names(factorCheck)[!factorCheck])
            x[[i]] <- as.factor(x[[i]])
    }
    if (is.factor(x$period)) {
      x$period <- as.integer(as.character(x$period))
    }

    if (periodClass == "integer")
        x$period <- as.integer(x$period)
    if (periodClass == "POSIXct") {
        if (!("POSIXct" %in% attr(x$period, "class")))
            x$period <- ISOyear(x$period)
    }
    if (!is.numeric(x$value))
        stop("Value column must contain numeric data!")

    # rearrange data for better readability
    reorder <-
        c(mandatoryColumns[mandatoryColumns != "value"], names(x)[!(names(x) %in% mandatoryColumns)], "value")
    x <- x[reorder]

    # add NA entrys to factors
    if (addNA) x <- qaddNA(x)
    if (na.rm) x <- x[!is.na(x$value), ]

    attributes(x) <- c(attributes(x)[match(setdiff(names(attributes(x)),
                                                   names(storeAttributes)),
                                           names(attributes(x)))],
                       storeAttributes)
    class(x) <- c("quitte", class(x))
    return(x)
}

#' @method as.quitte magpie
#' @export
as.quitte.magpie <- function(x, periodClass = "integer", addNA = FALSE, na.rm = FALSE) { # nolint
    if (!(periodClass %in% c("integer", "POSIXct")))
      stop("periodClass must be in c('integer', 'POSIXct')")

    x <- magclass::clean_magpie(x, what = "sets")
    if (magclass::getSets(x, fulldim = FALSE)[3] == "d3") {
      magclass::getSets(x, fulldim = FALSE)[3] <- "variable"
    }
    if (!("unit" %in% magclass::getSets(x))        &
 ("variable" %in% magclass::getSets(x))) {
      if (all(grepl(" \\(.*\\)$",
                    magclass::getNames(x, fulldim = TRUE)$variable))) {
        magclass::getNames(x) <- sub(" \\(([^\\()]*)\\)($|\\.)", ".\\1\\2",
                                     magclass::getNames(x))
        magclass::getSets(x, fulldim = FALSE)[3] <-
          sub("variable", "variable.unit",
              magclass::getSets(x, fulldim = FALSE)[3])
      }
    }

    d <- dimnames(x)
    if (!is.null(names(d)[[3]])) {
      datanames <- strsplit(names(d)[[3]], "\\.")[[1]]
      datanames <- make.unique(c("cell", "region", "year", "value", datanames),
                               sep = "")[- (1:4)]
    } else {
      datanames <- NULL
    }

    x <- magclass::as.data.frame(x)

    if (all(is.na(x$Cell)))
      x$Cell <- NULL # nolint

    if (length(datanames) > 0) {
      for (i in seq_along(datanames))
        colnames(x)[colnames(x) == paste0("Data", i)] <- datanames[i]
    } else {
      if ("Data1" %in% colnames(x))
        if (all(levels(x$Data1) == "NA"))
          x$Data1 <- NULL # nolint
    }

    quitteColumns <- c("model", "scenario", "region", "variable", "unit",
                        "period", "value")

    for (cn in quitteColumns) {
      colnames(x)[tolower(colnames(x)) == cn] <- cn
    }
    colnames(x)[tolower(colnames(x)) == "year"] <- "period"

    if (!is.factor(x$region))
      x$region <- fct_na_value_to_level(as.factor(x$region),
                                        level = '(Missing)')

    if (all(x$period == 0)) {
      levels(x$period) <- NA
    } else if (periodClass == "integer") {
      x$period <- as.integer(as.character(x$period))
    } else if (periodClass == "POSIXct") {
      x$period <- ISOyear(x$period)
    }

    # add missing columns
    missingColumns <- setdiff(quitteColumns, colnames(x))

    if (length(missingColumns) > 0) {
      x <- data.frame(
        x,
        stats::setNames(
          as.list(rep(fct_na_value_to_level(factor(NA), level = '(Missing)'),
                      length(missingColumns))),
          missingColumns)
      )
    } else {
      x <- data.frame(x)
    }

    # reorder columns
    x <- x[c(match(quitteColumns, colnames(x)),
             match(setdiff(colnames(x), quitteColumns), colnames(x)))]

    # add NA entrys to factors
    if (addNA)
      x <- qaddNA(x)
    if (na.rm)
      x <- x[!is.na(x$value), ]
    x <- as_tibble(x)

    class(x) <- c("quitte", class(x))

    return(x)
  }

qaddNA <- function(x) {
  for (col in colnames(x)) {
    if (is.factor(x[[col]]))
      x[[col]] <- addNA(x[[col]], ifany = TRUE)
  }
  return(x)
}
