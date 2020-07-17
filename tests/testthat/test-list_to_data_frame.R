context('list_to_data_frame()')

test_that(
  'Test list_to_data_frame() results',
  {
    expect_equal(
      object = list_to_data_frame(
        l = list(Africa = c('Egypt', 'Tanzania'),
                 Europe = c('Portugal', 'Ukraine', 'Denmark')),
        region, country),
      expected = tibble(region = c('Africa', 'Europe')[c(1, 1, 2, 2, 2)],
                        country = c('Egypt', 'Tanzania', 'Portugal', 'Ukraine',
                                    'Denmark'))
    )
  })
