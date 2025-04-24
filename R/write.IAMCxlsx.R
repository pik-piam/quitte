#' Write .xlsx file
#'
#' Write a `.xlsx` file in line with IAMC standard.
#'
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
#' @importFrom dplyr bind_rows case_when rename_with setequal
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_title
#' @importFrom writexl write_xlsx
#' @export
write.IAMCxlsx <- function(x, path, append = FALSE) {
    x <- as.quitte(x)
    if (nrow(x) == 0) warning("Writing empty data frame to ", path)

    if (append && file.exists(path)) {
        other <- read.quitte(path)

        if (!setequal(colnames(x), colnames(other))) {
            stop('Cannot append to file `', path,
                 '` as columns differ from `x`.\n')
        }

        x <- bind_rows(x, other)
    }

    # convert to IAMC timeseries format and title case
    x <- x %>%
        pivot_periods() %>%
        rename_with(str_to_title)

    write_xlsx(list("data" = x), path)
}
