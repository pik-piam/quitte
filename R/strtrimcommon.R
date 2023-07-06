#' Trim common portions from both sides of a vector of strings
#'
#' @md
#' @param x A vector of strings
#' @param split A [`character`] to use for splitting.  If `split` is empty (i.e.
#'     `split  = ''`), `x` is split into single characters.  Otherwise, `x` is
#'     split on `split` boundaries.
#' @param USE.NAMES logical; if `TRUE` use `x` as [`names`] for the result.
#' @param return.all logical; if `FALSE` (the default), returns only the striped
#'     strings.  If `TRUE`, returns a list with elements `left`, `strings`,
#'     `right`, containing left and right common portions, and the trimmed
#'     strings, respectively.
#'
#' @return A (named) vector of strings, or a list of string vectors (see
#'     parameter `return.all` for details).
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
#' strtrimcommon(x, USE.NAMES = TRUE)
#'
#' x <- c('Some|name|with|common|text|elements',
#'        'Some|name|without|extra|text|elements')
#'
#' strtrimcommon(x, split = '|', return.all = TRUE)

#' @export
strtrimcommon <- function(x, split = '', USE.NAMES = FALSE,
                          return.all = FALSE) {
  split <- split[[1]]
  tmp <- lapply(strsplit(x, split, TRUE), rev)
  for (right in seq_len(min(sapply(tmp, length))))
    if (1 < length(unique(sapply(tmp, '[', i = right))))
      break

  tmp <- strsplit(x, split, TRUE)
  for (left in seq_len(min(sapply(tmp, length))))
    if (1 < length(unique(sapply(tmp, `[`, i = left))))
      break

  r <- sapply(tmp, function(x) {
    paste(x[left:(length(x) - right + 1)], collapse = split)
  })

  if (isTRUE(USE.NAMES)) {
    names(r) <- x
  }

  if (return.all) {
    return(list(
      left = paste(tmp[[1]][1:(left - 1)], collapse = split),
      strings = r,
      right = paste(tmp[[1]][length(tmp[[1]]) - rev(seq_len(right - 1)) + 1],
                    collapse = split)
    ))
  }
  else {
    return(r)
  }
}
