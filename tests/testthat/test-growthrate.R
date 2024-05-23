test_that("calc_growthrate works", {
  qe <- quitte_example_dataAR6
  expect_no_warning(qegr <- calc_growthrate(qe, only.new = TRUE))
  expect_equal(rbind(qe, qegr), calc_growthrate(qe, only.new = FALSE))
  expect_equal(sort(paste(levels(qe$variable), "[Growth Rate]")),
               sort(levels(qegr$variable)))
  expect_identical(levels(qegr$unit), "%/yr")
  expect_true(! min(qe$period) %in% qegr$period)
  expect_true(  max(qe$period) %in% qegr$period)
  # check calculations
  mifdata <- quitte::as.quitte(data.frame(
    "model"    = "REMIND",
    "scenario" = c("Base", "Base", "Base", "NDC", "NDC", "NDC"),
    "variable" = "Price|Energy",
    "value"    = c( 1.,     3.,     27.,    1.,    1024., 2.^30),
    "period"   = c( 2005,   2006,   2008,   2100,  2110,  2130),
    "region"   = "GLO",
    "unit"     = "US$2010/GJ"
  ))
  expect_no_warning(mifdatagr <- calc_growthrate(mifdata, only.new = TRUE))
  expect_equal(rbind(mifdata, mifdatagr), calc_growthrate(mifdata, only.new = FALSE))
  expect_equal(levels(mifdatagr$variable), "Price|Energy [Growth Rate]")
  base <- filter(mifdatagr, .data$scenario == "Base")
  expect_true(nrow(base) == 2)
  expect_equal(base$period, c(2006, 2008))
  expect_equal(base$value, c(200., 200.)) # tripling = 200% growth
  ndc <- filter(mifdatagr, .data$scenario == "NDC")
  expect_true(nrow(ndc) == 2)
  expect_equal(ndc$period, c(2110, 2130))
  expect_equal(ndc$value, c(100., 100.))  # doubling = 100% growth
  expect_equal(droplevels(base),
               calc_growthrate(mifdata, only.new = TRUE,
                               filter.function = function(x) filter(x, scenario == "Base")))
  expect_equal(nrow(calc_growthrate(mifdata, only.new = TRUE, filter.function = "whatever")), 0)


  expect_no_warning(alldata <- calc_growthrate(quitte_example_data, only.new = FALSE,
                    filter.function = function(x) filter(x, grepl("GDP", .data$variable))))
  expect_no_warning(GDPgrowth <- calc_growthrate(quitte_example_data, only.new = TRUE, filter.function = "GDP|PPP"))
  expect_identical(quitteSort(droplevels(GDPgrowth)),
                   droplevels(quitteSort(filter(alldata, grepl("Growth Rate", .data$variable)))))
})
