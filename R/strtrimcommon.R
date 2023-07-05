#' Trim common characters from both sides of a vector of strings
#'
#' @md
#' @param x A vector of strings
#' @param USE.NAMES logical; if `TRUE` use `x` as [`names`] for the result
#'
#' @return A (named) vector of strings.
#'
#' @author Michaja Pehl
#'
#' @examples
#' x <- c('/tmp/remind2_test-convGDX2MIF_fulldata.gdx',
#'        '/tmp/remind2_test-Ariadne_fulldata.gdx',
#'        '/tmp/remind2_test-NAVIGATE_fulldata.gdx',
#'        '/tmp/remind2_test-NGFS_fulldata_oneRegi.gdx',
#'        '/tmp/remind2_test-SHAPE_fulldata.gdx')
#'
#' strtrimcommon(x)

#' @export
strtrimcommon <- function(x, USE.NAMES = FALSE) {
  tmp <- lapply(x, function(x) { unlist(strsplit(x, '', TRUE)) })
  for (left in seq_len(min(sapply(tmp, length))))
    if (1 < length(unique(sapply(tmp, `[`, i = left))))
      break

  tmp <- lapply(x, function(x) { rev(unlist(strsplit(x, '', TRUE))) })
  for (right in seq_len(min(sapply(tmp, length))))
    if (1 < length(unique(sapply(tmp, '[', i = right))))
      break

  sapply(x, function(x) {
    substr(x, start = left, stop = nchar(x) - right + 1)
  }, USE.NAMES = USE.NAMES)
}

