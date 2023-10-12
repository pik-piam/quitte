# test column existence ----
test_that(
  desc = 'is_quitte() returns FALSE on missing columns',
  code = {
    for (col in colnames(quitte_example_data))
      expect_false(
        is_quitte(select(quitte_example_data, -all_of(col))))
  }
)

# test additional columns ----
test_that(
  desc = 'is_quitte() returns TRUE on additional columns',
  code = {
    expect_true(is_quitte(quitte_example_data %>% mutate(foo = n())))
  }
)

# test wrong column classes ----
test_that(
  desc = 'is_quitte() returns FALSE on wrong column classes',
  code = {
    x <- quitte_example_data %>%
      factor.data.frame()
    for (col in colnames(quitte_example_data)) {
      if (col %in% c('model', 'scenario', 'region', 'variable', 'unit')) {
        expect_false(
          is_quitte(mutate(x, !!sym(col) := as.integer(.data[[col]]))))
      }
      else if ('period' == col) {
        expect_false(is_quitte(mutate(x, period = is.numeric(x[['period']]))))
      }
      else {
        expect_false(is_quitte(mutate(x, value = as.character(x[['value']]))))
      }
    }
  }
)

# test correct column classes ----
test_that(
  desc = 'is_quitte() returns TRUE on correct column classes',
  code = {
    expect_true(is_quitte(quitte_example_data %>% factor.data.frame()))
    expect_true(is_quitte(quitte_example_data %>% character.data.frame()))
    expect_true(
      is_quitte(quitte_example_data %>%
                  mutate(period = ISOyear(.data[['period']]))))
      }
)
