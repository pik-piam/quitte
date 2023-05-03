context('chooseFilter()')

test_that('select data', {
  qe <- as.quitte(quitte_example_dataAR6, na.rm = TRUE)

  actual <- chooseFilter(qe, types = NULL, keep = NULL)
  expect_identical(actual, qe)

  actual <- chooseFilter(qe, types = "scenario", keep = list("scenario" = levels(qe$scenario)))
  expect_identical(actual, qe)

  expect_warning(chooseFilter(qe, types = "scenario", keep = list("scenario" = c(levels(qe$scenario), "Apocalypse"))),
                 "Data does not contain scenario Apocalypse")

  qeF <- droplevels(dplyr::filter(qe, .data$model == levels(.data$model)[[1]]))
  actual <- chooseFilter(qeF, types = "model", keep = NULL)
  expect_identical(actual, qeF)

  expect_warning(chooseFilter(qeF, types = "model", keep = list("model" = "WHATEVER")),
                 "Data does not contain model WHATEVER")
})
