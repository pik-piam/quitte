context('duplicate()')

test_that(
  'Test duplicate() results',
  {
    data <- tibble(region   = rep(c('AFR', 'CHN'), 2),
                   variable = paste('Var', c(1, 1, 2, 2)),
                   value    = 1:4)

    expect_equal(
      object = duplicate(data, region = 'World'),
      expected = bind_rows(data, mutate(data, region = 'World'))
    )
  })
