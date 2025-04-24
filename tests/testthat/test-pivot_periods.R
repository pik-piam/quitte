test_that(
    'pivot_periods() functions work as expected',
    {
        expect_identical(
            object = quitte_example_data %>%
                pivot_periods_wider() %>%
                pivot_periods_longer(),
            expected = quitte_example_data)

        expect_identical(
            object = quitte_example_data %>%
                pivot_periods_wider(),
            expected = quitte_example_data %>%
                pivot_periods())

        expect_identical(
            object = quitte_example_data %>%
                pivot_periods_wider() %>%
                pivot_periods_longer(),
            expected = quitte_example_data %>%
                pivot_periods_wider() %>%
                pivot_periods())
    }
)

test_that(
    'pivot_periods() functions throw errors as expected',
    {
        expect_error(
            object = quitte_example_data %>%
                pivot_periods_longer(),
            regexp = 'No columns with names matching integer numbers in `df`')

        expect_error(
            object = quitte_example_data %>%
                pivot_periods_wider() %>%
                pivot_periods_wider(),
            regexp = 'No `period` column in `df`')
    })
