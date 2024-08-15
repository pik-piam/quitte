test_that("report duplicate works", {
  # really identical
  d <- data.frame(variable = "Emi", unit = "Mt CO2/yr", period = c(2020, 2020), value = 1)
  expect_warning(dupl <- reportDuplicates(d),
                 "Emi")
  expect_true(nrow(dupl) > 0)
  # difference in unit only
  d <- data.frame(variable = "Emi", unit = c("Mt CO2/yr", "kt CO2/yr"), period = 2020, value = 1)
  expect_warning(dupl <- reportDuplicates(d),
                 "Emi")
  expect_true(nrow(dupl) > 0)
  # difference in value and unit
  d <- data.frame(variable = "Emi", unit = c("Mt CO2/yr", "kt CO2/yr"), period = 2020, value = c(1, 2))
  expect_warning(dupl <- reportDuplicates(d),
                 "Emi")
  expect_true(nrow(dupl) > 0)
  # everything fine
  d <- data.frame(variable = "Emi", unit = "Mt CO2/yr", period = 2020, value = 1)
  expect_no_warning(dupl <- reportDuplicates(d))
  expect_true(nrow(dupl) == 0)
  # everything fine with exampledata
  expect_no_warning(dupl <- reportDuplicates(quitte_example_data))
  expect_true(nrow(dupl) == 0)
  expect_no_warning(dupl <- reportDuplicates(quitte_example_dataAR6))
  expect_true(nrow(dupl) == 0)
})
