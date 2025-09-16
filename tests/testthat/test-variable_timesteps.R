context('variable timesteps')

#  parameters ----
## add_timesteps_columns() parameters ----
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

## add_remind_timesteps_columns() parameters ----
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

## ggplot_bar_vts() parameters ----
test_that(
  desc = 'ggplot_bar_vts() parameters',
  code = {
    expect_error(
      object = ggplot_bar_vts(data = NULL,
                              timesteps = remind_timesteps,
                              mapping = aes(x = !!sym('period'),
                                            y = !!sym('value'),
                                            fill = !!sym('variable'))),
      regexp = '`data` must be a data frame.')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = NULL,
                              mapping = aes(x = !!sym('period'),
                                            y = !!sym('value'),
                                            fill = !!sym('variable'))),
      regexp = '`timesteps` must be a data frame.')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = remind_timesteps,
                              mapping = aes(y = !!sym('value'),
                                            fill = !!sym('variable'))),
      regexp = 'Mapping x is missing.')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = remind_timesteps,
                              mapping = aes(x = !!sym('period'),
                                            fill = !!sym('variable'))),
      regexp = 'Mapping y is missing.')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = remind_timesteps,
                              mapping = aes(x = !!sym('period'),
                                            y = !!sym('value'))),
      regexp = 'Mapping fill is missing.')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = remind_timesteps,
                              mapping = aes(fill = !!sym('variable'))),
      regexp = 'Mappings x, y are missing.')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = remind_timesteps,
                              mapping = aes(x = !!sym('x'),
                                            y = !!sym('value'),
                                            fill = !!sym('variable'))),
      regexp = 'Column `x` is missing in `data`.')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = remind_timesteps,
                              mapping = aes(x = !!sym('period'),
                                            y = !!sym('y'),
                                            fill = !!sym('variable'))),
      regexp = 'Column `y` is missing in `data`.')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = remind_timesteps,
                              mapping = aes(x = !!sym('period'),
                                            y = !!sym('value'),
                                            fill = !!sym('fill'))),
      regexp = 'Column `fill` is missing in `data`.')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = remind_timesteps,
                              mapping = aes(x = !!sym('x'),
                                            y = !!sym('value'),
                                            fill = !!sym('fill'))),
      regexp = 'Columns `x`, `fill` are missing in `data`')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = data.frame(year = 0,
                                                     weight = 0),
                              mapping = aes(x = !!sym('period'),
                                            y = !!sym('value'),
                                            fill = !!sym('variable'))),
      regexp = 'Column `period` is missing in `timesteps`')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = data.frame(period = 0,
                                                     weight = 0),
                              mapping = aes(x = !!sym('period'),
                                            y = !!sym('value'),
                                            fill = !!sym('variable'))),
      regexp = 'Column `year` is missing in `timesteps`.')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = data.frame(period = 0,
                                                     year = 0),
                              mapping = aes(x = !!sym('period'),
                                            y = !!sym('value'),
                                            fill = !!sym('variable'))),
      regexp = 'Column `weight` is missing in `timesteps`.')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = data.frame(period = 0),
                              mapping = aes(x = !!sym('period'),
                                            y = !!sym('value'),
                                            fill = !!sym('variable'))),
      regexp = 'Columns `year`, `weight` are missing in `timesteps`.')

    expect_error(
      object = ggplot_bar_vts(data = quitte_example_data,
                              timesteps = remind_timesteps,
                              mapping = aes(x = !!sym('period'),
                                            y = !!sym('value'),
                                            fill = !!sym('variable')),
                              gaps = 'a'),
      regexp = '`gaps` must be a positive numerical.')
  }
)

## ggplot_bar_remind_vts() parameters ----
test_that(
  desc = 'ggplot_bar_remind_vts() parameters',
  code = {
    expect_error(
      object = ggplot_bar_remind_vts(data = NULL,
                                     mapping = aes(x = !!sym('period'),
                                                   y = !!sym('value'),
                                                   fill = !!sym('variable'))),
      regexp = '`data` must be a data frame.')

    expect_error(
      object = ggplot_bar_remind_vts(data = quitte_example_data,
                                     mapping = aes(y = !!sym('value'),
                                                   fill = !!sym('variable'))),
      regexp = 'Mapping x is missing.')

    expect_error(
      object = ggplot_bar_remind_vts(data = quitte_example_data,
                                     mapping = aes(x = !!sym('period'),
                                                   fill = !!sym('variable'))),
      regexp = 'Mapping y is missing.')

    expect_error(
      object = ggplot_bar_remind_vts(data = quitte_example_data,
                                     mapping = aes(x = !!sym('period'),
                                                   y = !!sym('value'))),
      regexp = 'Mapping fill is missing.')

    expect_error(
      object = ggplot_bar_remind_vts(data = quitte_example_data,
                                     mapping = aes(fill = !!sym('variable'))),
      regexp = 'Mappings x, y are missing.')

    expect_error(
      object = ggplot_bar_remind_vts(data = quitte_example_data,
                                     mapping = aes(x = !!sym('x'),
                                                   y = !!sym('value'),
                                                   fill = !!sym('variable'))),
      regexp = 'Column `x` is missing in `data`.')

    expect_error(
      object = ggplot_bar_remind_vts(data = quitte_example_data,
                                     mapping = aes(x = !!sym('period'),
                                                   y = !!sym('y'),
                                                   fill = !!sym('variable'))),
      regexp = 'Column `y` is missing in `data`.')

    expect_error(
      object = ggplot_bar_remind_vts(data = quitte_example_data,
                                     mapping = aes(x = !!sym('period'),
                                                   y = !!sym('value'),
                                                   fill = !!sym('fill'))),
      regexp = 'Column `fill` is missing in `data`.')

    expect_error(
      object = ggplot_bar_remind_vts(data = quitte_example_data,
                                     mapping = aes(x = !!sym('x'),
                                                   y = !!sym('value'),
                                                   fill = !!sym('fill'))),
      regexp = 'Columns `x`, `fill` are missing in `data`')

    expect_error(
      object = ggplot_bar_remind_vts(data = quitte_example_data,
                                     mapping = aes(x = !!sym('period'),
                                                   y = !!sym('value'),
                                                   fill = !!sym('variable')),
                                     gaps = 'a'),
      regexp = '`gaps` must be a positive numerical.')
  }
)

# calculations ----
## add_timesteps_columns() calculations ----
test_that(
  desc = 'add_timesteps_columns() calculations',
  code = {
    # no gaps, centred on period
    expect_equal(
      object = add_timesteps_columns(
        data = quitte_example_data %>%
          filter(first(scenario) == scenario,
                 last(region) == region,
                 'Consumption' == variable),
        timesteps = remind_timesteps,
        periods = 'period',
        gaps = 0) %>%
        select('period', 'xpos', 'width') %>%
        as.data.frame(),
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
        select('period', 'xpos', 'width') %>%
        as.data.frame(),
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

test_that(
  desc = 'the arguments of add_timesteps_columns() with default values work',
  code = {
    timesteps <- data.frame(tau = c(rep(1,2),rep(2,4), rep(3,1)),
                            hour = 1:7,
                            weight = 1)
    data <- data.frame(tau = 1:3)

    object <- add_timesteps_columns(data, timesteps,
                                    periods = 'tau',
                                    gaps = 0.1,
                                    interval_shift = c(-1, 0),
                                    timesteps_period = 'tau',
                                    timesteps_interval = 'hour')

    expected <- data.frame(tau = 1:3,
                           xpos = c(1, 4, 6.5),
                           width = c(1.9, 3.9, 0.9))

    expect_equal(object, expected, tolerance = testthat_tolerance())
  }
)
## add_remind_timesteps_columns() calculations ----
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
        periods = 'period',
        gaps = 0) %>%
        select('period', 'xpos', 'width') %>%
        as.data.frame(),
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
        select('period', 'xpos', 'width') %>%
        as.data.frame(),
      expected = data.frame(
        period = c(2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040,
                   2045, 2050, 2055, 2060, 2070, 2080, 2090, 2100,
                   2110, 2130, 2150),
        xpos = c(2005,   2010, 2015, 2020,    2025, 2030, 2035, 2040,
                 2045,   2050, 2055, 2061.25, 2070, 2080, 2090, 2100,
                 2112.5, 2130, 2153.5),
        width = c(4.5, 4.5, 4.5, 4.5, 4.5, 4.5,  4.5,  4.5,  4.5, 4.5,
                  4.5, 7.0, 9.5, 9.5, 9.5, 9.5, 14.5, 19.5, 26.5)))

    # real-world bug, periods not named 'period'
    expect_equal(
      object = add_remind_timesteps_columns(
        data = data.frame(facet = 'specific energy use',
                          t     = 2005L,
                          pf    = factor('feso_otherInd'),
                          value = 0.0293,
                          stringsAsFactors = FALSE),
        periods = 't'),
      expected = add_remind_timesteps_columns(
        data = data.frame(facet  = 'specific energy use',
                          period = 2005L,
                          pf     = factor('feso_otherInd'),
                          value  = 0.0293,
                          stringsAsFactors = FALSE),
        periods = 'period') %>%
        rename(t = period))
  }
)

# plots ----
## ggplot_bar_vts() plots ----
test_that(
  desc = 'ggplot_bar_vts() plots',
  code = {
    expect_s3_class(
      object = ggplot_bar_vts(
        data = quitte_example_data %>%
          filter(first(scenario) == scenario,
                 last(region) == region,
                 first(variable) == variable),
        timesteps = remind_timesteps,
        mapping = ggplot2::aes(x = !!sym('period'),
                               y = !!sym('value'),
                               fill = !!sym('variable'))),
      class = 'ggplot')
  }
)

## ggplot_bar_remind_vts() plots ----
test_that(
  desc = 'ggplot_bar_remind_vts() plots',
  code = {
    expect_s3_class(
      object = ggplot_bar_remind_vts(
        data = quitte_example_data %>%
          filter(first(scenario) == scenario,
                 last(region) == region,
                 first(variable) == variable),
        mapping = ggplot2::aes(x = !!sym('period'),
                               y = !!sym('value'),
                               fill = !!sym('variable'))),
      class = 'ggplot')
  }
)
