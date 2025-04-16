
#' Write .xlsx file
#'
#' Write a `.xlsx` file in line with IAMC standard.
#'
#' @md
#' @param x A [`quitte`] data frame.
#' @param path Path or connection to write to.
#' @param append Overwrite existing files (`FALSE`, default), or append to them
#'   (`TRUE`).
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

    # convert to IAMC timeseries format
    if (!is.IAMCTimeseries)
        x <- as.IAMCTimeseries(df = x, fill = NA)

    if (append && file.exists(path)) {
        x <- rbind(read.quitte(path), x)
    }

    write_xlsx(list("data" = x), path)
}
