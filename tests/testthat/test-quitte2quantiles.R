context('quitte2quantiles()')

test_that(
  'Test quitte2quantiles() results',
  {
    expect_equal(
      object = quitte_example_data %>%
        filter('Consumption' == variable,
               2015 >= period) %>%
        quitte2quantiles(probs = c('min' = 0, 'max' = 1),
                         grouping = 'period'),
      expected = tibble(period = c(2005, 2010, 2015),
                        min = c(  565.8014602,   584.1255302,   676.5649172),
                        max = c(29337.297731,  31657.7326809, 38218.6406662))
    )
  })
