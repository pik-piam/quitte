test_that(
  desc = 'is_quitte() returns FALSE on missing columns',
  code = {
    for (x in colnames(quitte_example_data))
      expect_false(
        is_quitte(select(quitte_example_data, -all_of(x))))
  }
)
