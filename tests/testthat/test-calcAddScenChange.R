# test data ----
d <- tribble(
    ~scenario,   ~variable,   ~unit,     ~region,  ~period,  ~value,
    'scen_A',    'Emissions', 'Mt',      'World',  2030,      10,
    'scen_A',    'GDP',       'bn USD',  'World',  2030,     200,
    'scen_A',    'Emissions', 'Mt',      'World',  2050,       8,
    'scen_A',    'GDP',       'bn USD',  'World',  2050,     160,
    'scen_REF',  'Emissions', 'Mt',      'World',  2030,       5,
    'scen_REF',  'GDP',       'bn USD',  'World',  2030,     100,
    'scen_REF',  'Emissions', 'Mt',      'World',  2050,       5,
    'scen_REF',  'GDP',       'bn USD',  'World',  2050,     100)

# input validation ----
test_that('d must be a data frame', {
    expect_error(
        calc_addScenIncrease(list(), 'Emissions', 'scen_REF'),
        class = 'rlang_error'
    )
})

test_that('d must have a scenario column', {
    expect_error(
        calc_addScenIncrease(rename(d, scen = 'scenario'),
                             'Emissions', 'scen_REF'),
        regexp = 'scenario'
    )
})

test_that('d must have a variable column', {
    expect_error(
        calc_addScenIncrease(rename(d, var = 'variable'),
                             'Emissions', 'scen_REF'),
        regexp = 'variable'
    )
})

test_that('d must have a unit column', {
    expect_error(
        calc_addScenIncrease(select(d, -'unit'),
                             'Emissions', 'scen_REF'),
        regexp = 'unit'
    )
})

test_that('d must have a value column', {
    expect_error(
        calc_addScenIncrease(select(d, -'value'),
                             'Emissions', 'scen_REF'),
        regexp = 'value'
    )
})

test_that('variable must exist in d$variable', {
    expect_error(
        calc_addScenIncrease(d, 'NonExistent', 'scen_REF'),
        regexp = 'NonExistent'
    )
})

test_that('reference_scenario must exist in d$scenario', {
    expect_error(
        calc_addScenIncrease(d, 'Emissions', 'scen_MISSING'),
        regexp = 'scen_MISSING'
    )
})

# calc_addScenIncrease ----
test_that('increase: values are correct', {
    result <- calc_addScenIncrease(d, 'Emissions', 'scen_REF')
    expect_equal(
        result |>
            filter(.data$scenario == 'scen_A') |>
            pull('value'),
        c(5, 3)   # 10 - 5, 8 - 5
    )
})

test_that('increase: variable name has correct suffix', {
    result <- calc_addScenIncrease(d, 'Emissions', 'scen_REF')
    expect_true(all(grepl('\\|Inc over ', result$variable)))
})

test_that('increase: unit is unchanged', {
    result <- calc_addScenIncrease(d, 'Emissions', 'scen_REF')
    expect_identical(unique(result$unit), 'Mt')
})

test_that('increase: reference scenario is not in output', {
    result <- calc_addScenIncrease(d, 'Emissions', 'scen_REF')
    expect_false('scen_REF' %in% result$scenario)
})

# calc_addScenRatio ----
test_that('ratio: values are correct', {
    result <- calc_addScenRatio(d, 'Emissions', 'scen_REF')
    expect_equal(
        result |> filter(.data$scenario == 'scen_A') |> pull('value'),
        c(2, 1.6)   # 10/5, 8/5
    )
})

test_that('ratio: variable name has correct suffix', {
    result <- calc_addScenRatio(d, 'Emissions', 'scen_REF')
    expect_true(all(grepl('\\|Ratio to ', result$variable)))
})

test_that('ratio: unit is set to ratio', {
    result <- calc_addScenRatio(d, 'Emissions', 'scen_REF')
    expect_identical(unique(result$unit), 'ratio')
})

# calc_addScenPercentChange ----
test_that('percent change: values are correct', {
    result <- calc_addScenPercentChange(d, 'Emissions', 'scen_REF')
    expect_equal(
        result |> filter(.data$scenario == 'scen_A') |> pull('value'),
        c(100, 60)   # (10/5 - 1)*100, (8/5 - 1)*100
    )
})

test_that('percent change: variable name has correct suffix', {
    result <- calc_addScenPercentChange(d, 'Emissions', 'scen_REF')
    expect_true(all(grepl('\\|% Change to', result$variable)))
})

test_that('percent change: unit is set to %', {
    result <- calc_addScenPercentChange(d, 'Emissions', 'scen_REF')
    expect_identical(unique(result$unit), '%')
})

# reference_short_name ----
test_that('reference_short_name appears in variable name', {
    result <- calc_addScenIncrease(d, 'Emissions', 'scen_REF',
                                   reference_short_name = 'REF')
    expect_true(all(grepl('REF', result$variable)))
})

test_that('full scenario name is not used when reference_short_name is set', {
    result <- calc_addScenIncrease(d, 'Emissions', 'scen_REF',
                                   reference_short_name = 'REF')
    expect_false(any(grepl('scen_REF', result$variable)))
})

# Multi-variable ----
test_that('multiple variables can be passed at once', {
    result <- calc_addScenIncrease(d, c('Emissions', 'GDP'), 'scen_REF')
    expect_length(result$variable |> unique(), 2)
})

test_that('only requested variables appear in output', {
    result <- calc_addScenIncrease(d, 'Emissions', 'scen_REF')
    expect_false(any(grepl('GDP', result$variable)))
})

# Output structure ----
test_that('output has no leftover reference_short_name column', {
    result <- calc_addScenIncrease(d, 'Emissions', 'scen_REF',
                                   reference_short_name = 'REF')
    expect_false('REF' %in% colnames(result))
})

test_that('output columns match input columns', {
    result <- calc_addScenIncrease(d, 'Emissions', 'scen_REF')
    expect_setequal(colnames(result), colnames(d))
})
