context('interpolate_missing_periods()')

test_that(
  'Test interpolate_missing_periods()',
  {
    data <- tibble(period   = 2000:2004,
                   variable = 'a',
                   value    = sqrt(1:5))

    expect_equal(
        object = interpolate_missing_periods_(
            data = data, periods = list(period = 2002:2005)) %>%
            arrange(period),

        expected = bind_rows(
            data,

            tibble(period = 2005, variable = 'a', value = NA_real_)
        )
    )

    data <- tibble(period = 2004, variable = 'a', value = 1)

    expect_equal(
        object = interpolate_missing_periods_(
            data = data, periods = list(period = 2002:2005)) %>%
            arrange(period),

        expected = bind_rows(
            tibble(period = 2002:2003, variable = 'a', value = NA_real_),
            data,
            tibble(period = 2005,      variable = 'a', value = NA_real_)
        )
    )
  })
