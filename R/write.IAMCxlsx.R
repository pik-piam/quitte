
#' Write .xlsx file
#'
#' Write a `.xlsx` file in line with IAMC standard.
#'
#' @md
#' @param x A [`quitte`] data frame.
#' @param path Path or connection to write to.
#' @param append Overwrite existing files (`FALSE`, default), or append to them
#'   (`TRUE`).
#' @param is.IAMCTimeseries Set TRUE if data frame has already been converted to
#'  wide format. Defaults to FALSE.
#'
#' @author Michaja Pehl, Oliver Richters
#'
#' @examples
#' write.IAMCxlsx(quitte_example_data, tempfile())
#'
#' @importFrom writexl write_xlsx
#' @export
write.IAMCxlsx <- function(x, path, append = FALSE, is.IAMCTimeseries = FALSE) {
    x <- as.quitte(x)
    if (nrow(x) == 0) warning("Writing empty data frame to ", path)

    if (append && file.exists(path)) {
        other <- read.quitte(path)
        # If original df is already in wide format, we need to cast other as well
        if (is.IAMCTimeseries)
            other <- other %>% as.IAMCTimeseries()
        extra_cols <- setdiff(colnames(other), colnames(x))
        if(length(extra_cols) > 0)
            stop("Cannot append to file ", path, " as it contains extra columns: ", paste(extra_cols, collapse = ", "))
        x <- rbind(other, x)
    }

    # convert to IAMC timeseries format
    if (!is.IAMCTimeseries)
        x <- as.IAMCTimeseries(df = x, fill = NA)

    write_xlsx(list("data" = x), path)
}
