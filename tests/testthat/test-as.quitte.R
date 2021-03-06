context("as.quitte()")

test_that(
  "Test if dimensionless variables are transformed correctly",
  {
    a <- magclass::new.magpie(cells_and_regions = c("AFR","CHN"),
                              years = c(2010,2020),
                              fill = 1,
                              names = c("REMIND.VARIABLE ()",
                                        "REMIND.VARIABLE2 (Unit2)"),
                              sets = c("region", "year", "model", "variable"))

    expect_length(
      unique(as.quitte(a)[["period"]]),
      length(magclass::getYears(a)))

    expect_length(
      unique(as.quitte(a)[["region"]]),
      length(magclass::getRegions(a)))

    expect_length(
      unique(as.quitte(a)[["variable"]]),
      length(magclass::getNames(a)))

    expect_length(
      unique(as.quitte(a)[["unit"]]),
      length(magclass::getNames(a)))
  }
)

test_that(
  'Test if missing columns do not cause implicit <NA>s in as.quitte.magpie()',
  {
    a <- magclass::new.magpie(cells_and_regions = c("AFR","CHN"),
                              years = c(2010,2020),
                              fill = 1,
                              names = c("REMIND.VARIABLE ()",
                                        "REMIND.VARIABLE2 (Unit2)"),
                              sets = c("region", "year", "model", "variable"))
    # missing columns (scenario) should not have implicit <NA>s, since
    # tidyverse warns about them
    expect_false(any(is.na(levels(as.quitte(a)[['scenario']]))))
  }
)

test_that(
  paste('Test if missing columns do not cause implicit <NA>s in',
        'as.quitte.data.frame()'),
  {
    a <- crossing(model    = 'REMIND',
                  region   = c('AFR', 'CHN'),
                  variable = c('VARIABLE', 'VARIABLE2'),
                  period   = c(2010, 2020),
                  value    = 1) %>%
      mutate(unit = ifelse('VARIABLE' == variable, '', 'Unit2'))
    # missing columns (scenario) should not have implicit <NA>s, since
    # tidyverse warns about them
    expect_false(any(is.na(levels(as.quitte(a)[['scenario']]))))
  }
)
