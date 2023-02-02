context('calc_quantiles()')

test_that(
  'Test calc_quantiles() results',
  {
    data <- tibble(group = rep(c("A", "B"), 10),
                   value = 1:20) %>%
      arrange(group) %>%
      group_by(group)


    expect_equal(
      object = data %>%
        calc_quantiles() %>%
        pivot_wider(names_from = 'quantile') %>%
        ungroup(),
      expected = tribble(
        ~group, ~q0, ~q25, ~q50, ~q75, ~q100,
        'A',    1,   5.5,  10,   14.5, 19,
        'B',    2,   6.5,  11,   15.5, 20)
    )
  })
