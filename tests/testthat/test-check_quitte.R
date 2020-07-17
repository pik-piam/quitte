context('check_quitte()')

test_that(
  'Test check_quitte() results',
  {
    quitte <- rbind(
      data.frame(
        model    = "REMIND",
        scenario = "Baseline",
        region   = c("World", "USA", "EUR"),
        variable = "GDP",
        unit     = "US$2005",
        period   = 2005,
        value    = c(3, 1, 1)
      ),

      data.frame(
        model    = "REMIND",
        scenario = "Baseline",
        region   = "ROW",
        variable = c("FE|Total", "FE|Solids", "FE|Electricity"),
        unit     = "EJ/a",
        period   = 2005,
        value    = c(3, 1, 1)
      )
    )

    check_variables <- list(
      "FE|Total" = c("FE|Solids", "FE|Electricity"),
      "GDP"      = "GDP")

    check_regions <- paste0("World\nUSA\nEUR\n\nROW\nROW")

    expect_equal(
      object = check_quitte(quitte, check_variables, check_regions),
      expected = structure(
        list(
          model = structure(c(1L, 1L), .Label = "REMIND", class = "factor"),
          scenario = structure(c(1L, 1L), .Label = "Baseline", class = "factor"),
          region = c("ROW", "World"),
          variable = c("FE|Total", "GDP"),
          unit = structure(2:1, .Label = c("US$2005", "EJ/a"), class = "factor"),
          period = c(2005, 2005),
          value = c(3, 3), sum.value = c(2, 2)),
        row.names = c(NA, -2L),
        class = "data.frame")
    )
  })
