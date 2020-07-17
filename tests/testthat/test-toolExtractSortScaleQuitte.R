context('toolExtractSortScaleQuitte()')

test_that(
  'Test toolExtractSortScaleQuitte() results',
  {
    expect_equal(
      object = toolExtractSortScaleQuitte(
        x = quitte_example_data,
        scen = 'r7552c_1p5C_Def-rem-5',
        vars = c('Consumption', 'PE'),
        regi = c('EUR', 'LAM'),
        prd = c(2005, 2030, 2050)),
      expected = quitte_example_data %>%
        filter('r7552c_1p5C_Def-rem-5' == scenario,
               variable %in% c('Consumption', 'PE'),
               region %in% c('EUR', 'LAM'),
               period %in% c(2005, 2030, 2050)) %>%
        droplevels() %>%
        mutate(variable = factor(variable, levels = c('Consumption', 'PE'),
                                 ordered = TRUE))
    )
  })
