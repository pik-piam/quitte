context('variable timesteps')

# ---- > parameters < ----
# ---- add_timesteps_columns() parameters ----
test_that(
    desc = 'add_timesteps_columns() parameters',
    code = {
        expect_error(
            object = add_timesteps_columns(0, 0, 'period'),
            regexp = '`data` must be a data frame.')

        expect_error(
            object = add_timesteps_columns(data.frame(), 0, 'period'),
            regexp = '`timesteps` must be a data frame.')

        expect_error(
            object = add_timesteps_columns(data.frame(), remind_timesteps,
                                          'period'),
            regexp = 'Column `period` is missing in `data`.')

        x <- 'period'
        expect_error(
            object = add_timesteps_columns(data.frame(), remind_timesteps, x),
            regexp = 'Column `period` is missing in `data`.')
        rm(x)

        expect_error(
            object = add_timesteps_columns(data.frame(period = 0),
                                          data.frame(period = 0), 'period'),
            regexp = 'Columns `year`, `weight` are missing in `timesteps`.')

        expect_error(
            object = add_timesteps_columns(data.frame(x = 0), remind_timesteps,
                                          'x', gaps = 'a'),
            regexp = '`gaps` must be a positive numerical.')

        expect_error(
            object = add_timesteps_columns(data.frame(x = 0), remind_timesteps,
                                          'x', gaps = -1),
            regexp = '`gaps` must be a positive numerical.')

        # test different types of `periods` parameter
        expect_type(
            object = add_timesteps_columns(quitte_example_data,
                                           remind_timesteps, 'period'),
            type = 'list')

        expect_type(
            object = add_timesteps_columns(quitte_example_data,
                                           remind_timesteps, period),
            type = 'list')

        x <- 'period'
        expect_type(
            object = add_timesteps_columns(quitte_example_data,
                                           remind_timesteps, x),
            type = 'list')
        rm(x)

            expect_type(
            object = add_timesteps_columns(quitte_example_data,
                                           remind_timesteps),
            type = 'list')
    }
)

# ---- add_remind_timesteps_columns() parameters ----
test_that(
    desc = 'add_remind_timesteps_columns() parameters',
    code = {
        expect_error(
            object = add_remind_timesteps_columns(0, 'period'),
            regexp = '`data` must be a data frame.')

        expect_error(
            object = add_remind_timesteps_columns(data.frame(), 'period'),
            regexp = 'Column `period` is missing in `data`.')

        x <- 'period'
        expect_error(
            object = add_remind_timesteps_columns(data.frame(), x),
            regexp = 'Column `period` is missing in `data`.')
        rm(x)

        expect_error(
            object = add_remind_timesteps_columns(data.frame(period = 0),
                                                  'period', gaps = 'a'),
            regexp = '`gaps` must be a positive numerical.')

        expect_error(
            object = add_remind_timesteps_columns(data.frame(period = 0),
                                                  'period', gaps = -1),
            regexp = '`gaps` must be a positive numerical.')

        # test different types of `periods` parameter
        expect_type(
            object = add_remind_timesteps_columns(quitte_example_data,
                                                  'period'),
            type = 'list')

        expect_type(
            object = add_remind_timesteps_columns(quitte_example_data, period),
            type = 'list')

        x <- 'period'
        expect_type(
            object = add_remind_timesteps_columns(quitte_example_data, x),
            type = 'list')
        rm(x)

        expect_type(
            object = add_remind_timesteps_columns(quitte_example_data),
            type = 'list')
    }
)

# ---- ggplot_bar_vts() parameters ----
test_that(
    desc = 'ggplot_bar_vts() parameters',
    code = {
        expect_error(
            object = ggplot_bar_vts(data = NULL,
                                    timesteps = remind_timesteps,
                                    mapping = aes_string(x = 'period',
                                                         y = 'value',
                                                         fill = 'variable')),
            regexp = '`data` must be a data frame.')

        expect_error(
            object = ggplot_bar_vts(data = quitte_example_data,
                                    timesteps = NULL,
                                    mapping = aes_string(x = 'period',
                                                         y = 'value',
                                                         fill = 'variable')),
            regexp = '`timesteps` must be a data frame.')

        expect_error(
            object = ggplot_bar_vts(data = quitte_example_data,
                                    timesteps = remind_timesteps,
                                    mapping = aes_string(y = 'value',
                                                         fill = 'variable')),
            regexp = 'Mapping x is missing.')

        expect_error(
            object = ggplot_bar_vts(data = quitte_example_data,
                                    timesteps = remind_timesteps,
                                    mapping = aes_string(x = 'period',
                                                         fill = 'variable')),
            regexp = 'Mapping y is missing.')

        expect_error(
            object = ggplot_bar_vts(data = quitte_example_data,
                                    timesteps = remind_timesteps,
                                    mapping = aes_string(fill = 'variable')),
            regexp = 'Mappings x, y are missing.')

        expect_error(
            object = ggplot_bar_vts(data = quitte_example_data,
                                    timesteps = remind_timesteps,
                                    mapping = aes_string(x = 'x',
                                                         y = 'value',
                                                         fill = 'variable')),
            regexp = 'Column `x` is missing in `data`.')

        expect_error(
            object = ggplot_bar_vts(data = quitte_example_data,
                                    timesteps = remind_timesteps,
                                    mapping = aes_string(x = 'period',
                                                         y = 'y',
                                                         fill = 'variable')),
            regexp = 'Column `y` is missing in `data`.')

        expect_error(
            object = ggplot_bar_vts(data = quitte_example_data,
                                    timesteps = remind_timesteps,
                                    mapping = aes_string(x = 'period',
                                                         y = 'value',
                                                         fill = 'fill')),
            regexp = 'Column `fill` is missing in `data`.')

        expect_error(
            object = ggplot_bar_vts(data = quitte_example_data,
                                    timesteps = remind_timesteps,
                                    mapping = aes_string(x = 'x',
                                                         y = 'value',
                                                         fill = 'fill')),
            regexp = 'Columns `x`, `fill` are missing in `data`')

        expect_error(
            object = ggplot_bar_vts(data = quitte_example_data,
                                    timesteps = data.frame(year = 0,
                                                           weight = 0),
                                    mapping = aes_string(x = 'period',
                                                         y = 'value',
                                                         fill = 'variable')),
            regexp = 'Column `period` is missing in `timesteps`')

        expect_error(
            object = ggplot_bar_vts(data = quitte_example_data,
                                    timesteps = data.frame(period = 0,
                                                           weight = 0),
                                    mapping = aes_string(x = 'period',
                                                         y = 'value',
                                                         fill = 'variable')),
            regexp = 'Column `year` is missing in `timesteps`.')

        expect_error(
            object = ggplot_bar_vts(data = quitte_example_data,
                                    timesteps = data.frame(period = 0,
                                                           year = 0),
                                    mapping = aes_string(x = 'period',
                                                         y = 'value',
                                                         fill = 'variable')),
            regexp = 'Column `weight` is missing in `timesteps`.')

        expect_error(
            object = ggplot_bar_vts(data = quitte_example_data,
                                    timesteps = data.frame(period = 0),
                                    mapping = aes_string(x = 'period',
                                                         y = 'value',
                                                         fill = 'variable')),
            regexp = 'Columns `year`, `weight` are missing in `timesteps`.')

        expect_error(
            object = ggplot_bar_vts(data = quitte_example_data,
                                    timesteps = remind_timesteps,
                                    mapping = aes_string(x = 'period',
                                                         y = 'value',
                                                         fill = 'variable'),
                                    gaps = 'a'),
            regexp = '`gaps` must be a positive numerical.')
    }
)

# ---- ggplot_bar_remind_vts() parameters ----
test_that(
    desc = 'ggplot_bar_remind_vts() parameters',
    code = {
        expect_error(
            object = ggplot_bar_remind_vts(data = NULL,
                                    mapping = aes_string(x = 'period',
                                                         y = 'value',
                                                         fill = 'variable')),
            regexp = '`data` must be a data frame.')

        expect_error(
            object = ggplot_bar_remind_vts(data = quitte_example_data,
                                    mapping = aes_string(y = 'value',
                                                         fill = 'variable')),
            regexp = 'Mapping x is missing.')

        expect_error(
            object = ggplot_bar_remind_vts(data = quitte_example_data,
                                    mapping = aes_string(x = 'period',
                                                         fill = 'variable')),
            regexp = 'Mapping y is missing.')

        expect_error(
            object = ggplot_bar_remind_vts(data = quitte_example_data,
                                    mapping = aes_string(fill = 'variable')),
            regexp = 'Mappings x, y are missing.')

        expect_error(
            object = ggplot_bar_remind_vts(data = quitte_example_data,
                                    mapping = aes_string(x = 'x',
                                                         y = 'value',
                                                         fill = 'variable')),
            regexp = 'Column `x` is missing in `data`.')

        expect_error(
            object = ggplot_bar_remind_vts(data = quitte_example_data,
                                    mapping = aes_string(x = 'period',
                                                         y = 'y',
                                                         fill = 'variable')),
            regexp = 'Column `y` is missing in `data`.')

        expect_error(
            object = ggplot_bar_remind_vts(data = quitte_example_data,
                                    mapping = aes_string(x = 'period',
                                                         y = 'value',
                                                         fill = 'fill')),
            regexp = 'Column `fill` is missing in `data`.')

        expect_error(
            object = ggplot_bar_remind_vts(data = quitte_example_data,
                                    mapping = aes_string(x = 'x',
                                                         y = 'value',
                                                         fill = 'fill')),
            regexp = 'Columns `x`, `fill` are missing in `data`')

        expect_error(
            object = ggplot_bar_remind_vts(data = quitte_example_data,
                                    mapping = aes_string(x = 'period',
                                                         y = 'value',
                                                         fill = 'variable'),
                                    gaps = 'a'),
            regexp = '`gaps` must be a positive numerical.')
    }
)

# ---- > calculations < ----
# ---- add_timesteps_columns() calculations ----
test_that(
    desc = 'add_timesteps_columns() calculations',
    code = {
        # no gaps, centered on period
        expect_equal(
            object = add_timesteps_columns(
                data = quitte_example_data %>%
                    filter(first(scenario) == scenario,
                           last(region) == region,
                           'Consumption' == variable),
                timesteps = remind_timesteps,
                periods = 'period',
                gaps = 0) %>%
                select('period', 'xpos', 'width'),
            expected = data.frame(
                period = c(2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040,
                           2045, 2050, 2055, 2060, 2070, 2080, 2090, 2100,
                           2110, 2130, 2150),
                xpos = c(2005,   2010, 2015, 2020,    2025, 2030, 2035, 2040,
                         2045,   2050, 2055, 2061.25, 2070, 2080, 2090, 2100,
                         2112.5, 2130, 2153.5),
                width = c( 5,  5,  5,  5, 5, 5, 5, 5, 5, 5, 5, 7.5, 10, 10, 10,
                          10, 15, 20, 27)))

        # default gaps, same reduction (-0.5) in all widths
        expect_equal(
            object = add_timesteps_columns(
                data = quitte_example_data %>%
                    filter(first(scenario) == scenario,
                           last(region) == region,
                           'Consumption' == variable),
                timesteps = remind_timesteps,
                periods = 'period',
                gaps = 0.1) %>%
                select('period', 'xpos', 'width'),
            expected = data.frame(
                period = c(2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040,
                           2045, 2050, 2055, 2060, 2070, 2080, 2090, 2100,
                           2110, 2130, 2150),
                xpos = c(2005,   2010, 2015, 2020,    2025, 2030, 2035, 2040,
                         2045,   2050, 2055, 2061.25, 2070, 2080, 2090, 2100,
                         2112.5, 2130, 2153.5),
                width = c(4.5, 4.5, 4.5, 4.5, 4.5, 4.5,  4.5,  4.5,  4.5, 4.5,
                          4.5, 7.0, 9.5, 9.5, 9.5, 9.5, 14.5, 19.5, 26.5)))
    }
)

# ---- add_remind_timesteps_columns() calculations ----
test_that(
    desc = 'add_remind_timesteps_columns() calculations',
    code = {
        # no gaps, centered on period
        expect_equal(
            object = add_remind_timesteps_columns(
                data = quitte_example_data %>%
                    filter(first(scenario) == scenario,
                           last(region) == region,
                           'Consumption' == variable),
                period = 'period',
                gaps = 0) %>%
                select('period', 'xpos', 'width'),
            expected = data.frame(
                period = c(2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040,
                           2045, 2050, 2055, 2060, 2070, 2080, 2090, 2100,
                           2110, 2130, 2150),
                xpos = c(2005,   2010, 2015, 2020,    2025, 2030, 2035, 2040,
                         2045,   2050, 2055, 2061.25, 2070, 2080, 2090, 2100,
                         2112.5, 2130, 2153.5),
                width = c( 5,  5,  5,  5, 5, 5, 5, 5, 5, 5, 5, 7.5, 10, 10, 10,
                           10, 15, 20, 27)))

        # default gaps, same reduction (-0.5) in all widths
        expect_equal(
            object = add_remind_timesteps_columns(
                data = quitte_example_data %>%
                    filter(first(scenario) == scenario,
                           last(region) == region,
                           'Consumption' == variable),
                periods = 'period',
                gaps = 0.1) %>%
                select('period', 'xpos', 'width'),
            expected = data.frame(
                period = c(2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040,
                           2045, 2050, 2055, 2060, 2070, 2080, 2090, 2100,
                           2110, 2130, 2150),
                xpos = c(2005,   2010, 2015, 2020,    2025, 2030, 2035, 2040,
                         2045,   2050, 2055, 2061.25, 2070, 2080, 2090, 2100,
                         2112.5, 2130, 2153.5),
                width = c(4.5, 4.5, 4.5, 4.5, 4.5, 4.5,  4.5,  4.5,  4.5, 4.5,
                          4.5, 7.0, 9.5, 9.5, 9.5, 9.5, 14.5, 19.5, 26.5)))
    }
)

# ---- ggplot_bar_vts() plots ----
test_that(
    desc = 'ggplot_bar_vts() plots',
    code = {
        expect_warning(
            object = expect_s3_class(
                object = ggplot_bar_vts(
                    data = quitte_example_data %>%
                        filter(first(scenario) == scenario,
                               last(region) == region,
                               first(variable) == variable),
                    timesteps = remind_timesteps,
                    mapping = ggplot2::aes(x = period, y = value)),
                class = 'ggplot'),
            regexp = 'Ignoring unknown aesthetics: width')
    }
)

# ---- ggplot_bar_remind_vts() plots ----
test_that(
    desc = 'ggplot_bar_remind_vts() plots',
    code = {
        expect_warning(
            object = expect_s3_class(
                object = ggplot_bar_remind_vts(
                    data = quitte_example_data %>%
                        filter(first(scenario) == scenario,
                               last(region) == region,
                               first(variable) == variable),
                    mapping = ggplot2::aes(x = period, y = value)),
                class = 'ggplot'),
            regexp = 'Ignoring unknown aesthetics: width')
    }
)
