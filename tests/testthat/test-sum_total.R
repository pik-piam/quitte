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
  })
