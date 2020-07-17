context('add_countrycode()')

test_that(
  'Test add_countrycode results',
  {
    data <- tibble(
      country = c('Belgium', 'Narnia', 'Russia', 'Botswana'),
      data    = 1:4)

    expect_equal(
      object = add_countrycode(data, country = country.name, m49.code = un,
                               warn = FALSE),
      expected = data %>%
        mutate(m49.code = c(56, NA, 643, 72))
    )

    expect_equal(
      object = add_countrycode_(data, c('country' = 'country.name'), 'iso3c',
                                warn = FALSE, na.rm = TRUE),
      expected = data %>%
        filter('Narnia' != country) %>%
        mutate(iso3c = c('BEL', 'RUS', 'BWA'))
    )
  })

test_that(
  'Test add_countrycode warning message',
  {
    data <- tibble(
      country = c('Belgium', 'Narnia', 'Russia', 'Botswana'),
      data    = 1:4)

    expect_warning(
      object = add_countrycode(data, country = country.name, m49.code = un),
      regexp = 'Some values were not matched unambiguously: Narnia')
  })
