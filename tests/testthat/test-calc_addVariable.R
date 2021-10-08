context('calc_addVariable()')

library(tidyverse)

data <- inline.data.frame(c(
  "model;    scenario;   region;   variable;     unit;                 period;   value",
  "REMIND;   Baseline;   USA;      GDP|MER;      billion US$2005/yr;   2010;     12990",
  "REMIND;   Baseline;   USA;      Population;   million;              2010;       310.4",
  "REMIND;   Baseline;   USA;      PE;           EJ/yr;                2010;        91.62",
  "REMIND;   Baseline;   CHN;      GDP|MER;      billion US$2005/yr;   2020;      8882",
  "REMIND;   Baseline;   CHN;      GDP|MER;      billion US$2005/yr;   2010;      4119",
  "REMIND;   Baseline;   CHN;      Population;   million;              2020;      1387",
  "REMIND;   Baseline;   CHN;      Population;   million;              2010;      1349"))

test_that(
  'Test calc_addVariable() results',
  {
    result <- tibble(
      model    = 'REMIND',
      scenario = 'Baseline',
      region   = rep(c('USA', 'CHN', 'CHN'), 5)[1:13],
      variable = c('PE', rep(c('GDP|MER', 'Population', 'GDPpC', 'ln GDPpC'),
                             each = 3)),
      unit     = c('EJ/yr', rep(c('billion US$2005/yr', 'million',
                                  'US$2005/cap', NA), each = 3)),
      period   = rep(c(2010L, 2010L, 2020L), 5)[1:13],
      result = c(91.62,
                4119, 8882, 12990,
                1349, 1387, 310.4,
                3053.3728687917, 6403.74909877433, 41849.2268041237,
                8.02400211721046, 8.76463889451935, 10.6418286003377))

    expect_equal(
      object = full_join(
        calc_addVariable(data, "GDPpC" = "`GDP|MER` / Population * 1e3",
                         "`ln GDPpC`" = "log(GDPpC)",
                         units = c("US$2005/cap", NA)),
        result,
        c('model', 'scenario', 'region', 'variable', 'unit', 'period')
      ) %>%
        filter(1e-14 < 1 - result / value),
      expected = data %>%
        mutate(result = 0) %>%
        filter('REMIND' != model)
    )

    expect_equal(
      object = full_join(
        calc_addVariable_(
          data,
          list("`GDPpC`" = c("`GDP|MER` / `Population` * 1e3", "US$2005/cap"),
               "`ln GDPpC`" = "log(`GDPpC`)")),
        result,
        c('model', 'scenario', 'region', 'variable', 'unit', 'period')
      ) %>%
        filter(1e-14 < 1 - result / value),
      expected = data %>%
        mutate(result = 0) %>%
        filter('REMIND' != model)
    )

    expect_equal(
      object = tibble(model    = 'M',
                      scenario = c('A', 'A', 'B'),
                      region   = 'R',
                      variable = c('X', 'Y', 'X'),
                      unit     = 'U',
                      period   = 2020,
                      value    = c(1, 2, 4)) %>%
        calc_addVariable('Z' = 'X + Y', units = 'U', na.rm = FALSE,
                         completeMissing = TRUE),
      expected = tibble(model    = 'M',
                        scenario = rep(c('A', 'B'), each = 3),
                        region   = 'R',
                        variable = rep(c('X', 'Y', 'Z'), 2),
                        unit     = 'U',
                        period   = 2020,
                        value    = c(1:4, 0, 4))
    )
  })

test_that(
  'Test calc_addVariable() unit column substitution',
  {
    expect_equal(
      object = data %>%
        rename(foo = unit) %>%
        calc_addVariable("GDPpC" = "`GDP|MER` / Population * 1e3",
                         "`ln GDPpC`" = "log(GDPpC)",
                         units = c("US$2005/cap", NA),
                         unit = foo) %>%
        rename(unit = foo),
      expected = data %>%
        calc_addVariable("GDPpC" = "`GDP|MER` / Population * 1e3",
                         "`ln GDPpC`" = "log(GDPpC)",
                         units = c("US$2005/cap", NA))
        )
  })

test_that(
  desc = 'Test calc_addVariable() testing for duplicated data',
  code = {
    test_data <- quitte_example_data %>%
      filter(first(.data$model)    == .data$model,
             first(.data$scenario) == .data$scenario,
             first(.data$region)   == .data$region,
             first(.data$period)   == .data$period,
             .data$variable %in% c('PE', 'Population')) %>%
      `[`(c(1, 2, 2),)

    expect_error(
      object = calc_addVariable_(
        data = test_data,
        .dots = list('PE per capita' = c('PE / Population', unit = 'TJ/yr'))),
      regexp = 'Duplicate rows in data.frame.*')
  })
