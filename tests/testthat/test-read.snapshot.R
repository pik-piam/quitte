test_that("read.snapshot works", {
  qe <- quitte_example_dataAR6 %>%
    as.quitte(na.rm = TRUE) %>%
    filter(! variable == "Temperature|Global Mean") %>%
    quitteSort() %>%
    droplevels()
  tmpfile <- tempfile(pattern = "data", fileext = ".csv")
  write.table(pivot_wider(qe, names_from = "period"), 
              file = tmpfile, append = FALSE, quote = FALSE, sep = ",",
              eol = "\n", na = "", dec = ".", row.names = FALSE,
              col.names = TRUE) # mimick IIASA snapshot format
  expect_equal(qe, read.snapshot(tmpfile))
  if (Sys.which("sed") != "") {
    system(paste("sed -i 's/GCAM/\"GCAM\"/g;'", tmpfile))
    system(paste("sed -i 's/Delayed transition/\"Delayed transition\"/g;'", tmpfile))
  }
  rtests <- list(head(levels(qe$region), 1), head(levels(qe$region), 2))
  for (r in rtests) {
    expect_equal(droplevels(dplyr::filter(qe, region %in% r)),
                 read.snapshot(tmpfile, list(region = r)))
  }
  stests <- list(head(levels(qe$scenario), 1), head(levels(qe$scenario), 2))
  for (s in stests) {
    expect_equal(droplevels(dplyr::filter(qe, scenario %in% s)),
                 read.snapshot(tmpfile, list(scenario = s)))
  }
  vtests <- list(head(levels(qe$variable), 1), head(levels(qe$variable), 2), levels(qe$variable))
  for (v in vtests) {
    expect_equal(droplevels(dplyr::filter(qe, variable %in% v)),
                 read.snapshot(tmpfile, list(variable = v)))
  }
  ptests <- list(head(unique(qe$period), 1), head(unique(qe$period, 2)), unique(qe$period))  
  for (p in ptests) {
    expect_equal(droplevels(dplyr::filter(qe, period %in% p)),
                 read.snapshot(tmpfile, list(period = p)))
  }
  # test all jointly with last setting, test passing of filter.function
  filter.function <- function(x) droplevels(dplyr::filter(x, period %in% p, variable %in% v, region %in% r, scenario %in% s))
  snapshotdata <- read.snapshot(tmpfile, list(period = p, variable = v, region = r, scenario = s))
  expect_equal(filter.function(qe),
               snapshotdata)
  expect_equal(read.snapshot(tmpfile, filter.function = filter.function),
               snapshotdata)
})
