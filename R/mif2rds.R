
#' MIF2RDS converter
#'
#' Converts mif file into more memory efficient rds file. Additionally,
#' removes NA and duplicate entries.
#'
#' @param input Path to the MIF file to be converted
#' @param output File name of the output file
#' @author Jan Philipp Dietrich
#' @export

mif2rds <- function(input,output="output.rds") {
  a <- as.quitte(input)
  a <- a[!is.na(a$value),]
  a <- a[!duplicated(a),]
  saveRDS(a,output)
}
