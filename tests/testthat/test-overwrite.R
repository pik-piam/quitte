context('overwrite()')

test_that(
  'Test overwrite() results',
  {
    data <- data.frame(expand.grid(UPPER = LETTERS[1:2],
                                   lower = letters[24:26]),
                       value = 1:6)
    expect_equal(
      object = data %>%
        filter(lower == "y") %>%
        mutate(value = value * 10) %>%
        overwrite(data),
      expected = bind_rows(
        data %>%
          filter('y' == lower) %>%
          mutate(value = value * 10),

        data %>%
          filter('y' != lower))
    )
  })
