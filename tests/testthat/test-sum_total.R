context('sum_total()')

test_that(
  'Test sum_total() results',
  {
    d <- expand.grid(
      UPPER  = LETTERS[1:2],
      lower  = letters[24:26],
      number = 1:2
    ) %>%
      arrange(UPPER, lower, number) %>%
      mutate(value = c(1:6, NA, 8:12))

    expect_equal(
      object = sum_total(d, UPPER) %>%
        arrange(across(everything())),
      expected = bind_rows(
        d,
        d %>%
          group_by(lower, number) %>%
          summarise(value = sum(value, na.rm = TRUE),
                    UPPER = 'Total')
      ) %>%
        mutate(UPPER = as.factor(UPPER)) %>%
        arrange(across(everything()))
    )

    expect_equal(
      object = sum_total(d, lower, name = 'sum over lower', na.rm = FALSE) %>%
        arrange(across(everything())),
      expected = bind_rows(
        d,
        d %>%
          group_by(UPPER, number) %>%
          summarise(value = sum(value),
                    lower = 'sum over lower')
      ) %>%
        mutate(lower = factor(lower,
                              levels = c('x', 'y', 'z', 'sum over lower'))) %>%
        arrange(across(everything()))
    )

    e <- tibble(
      item = c('large', 'medium', 'small'),
      specific.value = c(1, 10, 100),
      size = c(1000, 100, 1))

    expect_equal(
      object = sum_total(e, item, value = specific.value, name = 'Average',
                         weight = size),
      expected = bind_rows(
        tibble(item = 'Average', specific.value = 2100 / 1101, size = 1101),
        e)
    )

    expect_equal(
      object = tibble(
        region = rep(c('US', 'EU28'), c(4, 6)),
        iso3c = rep(c('PRI', 'USA','ALA', 'AND', 'AUT'), each = 2),
        name = rep(c('EEK', 'kap'), 5),
        value = c(7.28297787369661e-07,
                  1.4825575525539,
                  5.47830041372924e-05,
                  196.700540269622,
                  7.1732435863781e-09,
                  0.00494520584427543,
                  1.98196459756622e-08,
                  0.0099356972852833,
                  9.80993573948498e-07,
                  0.955472798121879)) %>%
      sum_total(group = iso3c),
      expected = tibble(
        region = rep(c('EU28', 'US'), c(8, 6)),
        iso3c  = c(rep(c('ALA', 'AND', 'AUT', 'Total'), 2),
                   rep(c('PRI', 'Total', 'USA'), 2)),
        name = c(rep(c('EEK', 'kap'), each = 4),
                 rep(c('EEK', 'kap'), each = 3)),
        value = c(7.1732435863781e-09, 1.98196459756622e-08,
                  9.80993573948498e-07, 1.00798646351054e-06,
                  0.00494520584427543, 0.0099356972852833,
                  0.955472798121879, 0.970353701251438,
                  7.28297787369661e-07, 5.55113019246621e-05,
                  5.47830041372924e-05, 1.4825575525539,
                  198.183097822176, 196.700540269622)))
  }
)
