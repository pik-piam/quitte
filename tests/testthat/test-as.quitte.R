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
    expect_false(anyNA(levels(as.quitte(a)[['scenario']])))
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
    expect_false(anyNA(levels(as.quitte(a)[['scenario']])))
  }
)

test_that(
  'as.quitte can read csv files with , and ; as separator',
  {
    commafile <- tempfile(pattern = "comma", fileext = ".csv")
    writeLines(c("Model,Scenario,Region,Variable,Unit,2005,2010",
                 "REMIND,Base,World,FE,EJ/yr,12,14"),
               con = commafile, sep = "\n")
    semicolonfile <- tempfile(pattern = "semicolon", fileext = ".mif")
    writeLines(c("model;scenario;region;variable;unit;2005;2010",
                 "REMIND;Base;World;FE;EJ/yr;12;14"),
               con = semicolonfile, sep = "\n")
    expect_identical(as.quitte(commafile),
                     as.quitte(semicolonfile))
  }
)

test_that(
  'as.quitte understands lists',
  {
    expect_identical(quitte_example_data, as.quitte(list(list(quitte_example_data))))
    miffile <- tempfile(fileext = ".mif")
    writeLines(c("Model,Scenario,Region,Variable,Unit,2005,2010",
                 "REMIND,Base,World,FE,EJ/yr,12,14"),
               con = miffile, sep = "\n")
    expect_identical(as.quitte(miffile), as.quitte(list(miffile)))
  }
)

test_that(
  'as.quitte(NULL) works',
  {
    expect_true(is.quitte(as.quitte(NULL)))
    expect_equal(nrow(as.quitte(NULL)), 0)
    expect_equal(length(levels(as.quitte(NULL)$model)), 0)
  }
)

test_that(
  'as.quitte fails on non-supported filename',
  {
    expect_error(as.quitte('unknownfiletype.§§$%'),
                 'do not seem to be a valid file path: unknownfiletype.§§$%', fixed = TRUE)
  }
)

test_that(
  'as.quitte works on rds files',
  {
    rdsfile <- tempfile(pattern = "report", fileext = ".rds")
    saveRDS(quitte_example_data, rdsfile)
    expect_identical(as.quitte(rdsfile), quitte_example_data)
  }
)
