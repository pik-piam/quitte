# read.gdx() has a single (gamstransfer) backend, so the tests run directly.

gdx <- test_path('test_data/test.gdx')

# set up test data to compare to
source(test_path('test_data/make_test_data.R'))

identical_tibble <- function(x, y) {
    a <- all.equal(x, y)
    b <- all.equal(attributes(x)[sort(names(attributes(x)))],
                   attributes(y)[sort(names(attributes(y)))])

    if (!isTRUE(a)) {
        return(a)
    }
    else if (!isTRUE(b)) {
        return(b)
    }
    else {
        return(TRUE)
    }
}

## sets ----
test_that(
    'read.gdx() reads sets correctly',
    {
        x <- read.gdx(gdx, 'set_d1_lower')

        expect_true(tibble::is_tibble(x))
        expect_identical('foo', colnames(x))
        expect_identical(c('foo' = 'character'), sapply(x, class))
        expect_equal(length(set_d1_lower), nrow(x))
    })

test_that(
    'read.gdx() reads two-dimensional sets correctly',
    {
        x <- read.gdx(gdx, 'set_d2_alias')

        expect_true(tibble::is_tibble(x))

        expect_identical(c('set_d1_UPPER', 'set_d1_lower'), colnames(x))

        expect_identical(
            sapply(x, class),
            c('set_d1_UPPER' = 'character', 'set_d1_lower' = 'character'))

        expect_equal(prod(lengths(list(set_d1_UPPER, set_d1_lower))),
                     nrow(x))
    })

test_that(
    'read.gdx() reads set aliases correctly',
    {
        x <- read.gdx(gdx, 'set_d2')
        y <- read.gdx(gdx, 'set_d2_alias')

        expect_identical(x, y)
    })

test_that(
    'read.gdx() reads two-dimensional sets with identical defining sets',
    {
        x <- read.gdx(gdx, 'set_d2_identical')

        expect_identical(x, set_d2_identical)
    })


## parameters ----
test_that(
    'read.gdx() reads scalars correctly',
    {
        x <- read.gdx(gdx, 'parameter_d0')

        expect_equal(object = x,
                     expected = c('parameter_d0' = parameter_d0))
    })

test_that(
    'read.gdx() reads parameters correctly',
    {
        x <- read.gdx(gdx, 'parameter_d1')

        expect_true(identical_tibble(x, parameter_d1))
    })

test_that(
    'read.gdx() reads two-dimensional parameters correctly',
    {
        x <- read.gdx(gdx, 'parameter_d2')

        expect_true(identical_tibble(x, parameter_d2))
    })

test_that(
    'read.gdx() reads parameters with no defined values correctly',
    {
        x <- read.gdx(gdx, 'parameter_d2_0')

        expect_true(identical_tibble(x, parameter_d2_0))
    })

test_that(
    'read.gdx() keeps stored zeros and EPS by default (squeeze = FALSE)',
    {
        x <- read.gdx(gdx, 'parameter_d1_squeeze')

        expect_true(identical_tibble(x, parameter_d1_squeeze))
    })

test_that(
    'read.gdx() squeezes out stored zeros and EPS when squeeze = TRUE',
    {
        x <- read.gdx(gdx, 'parameter_d1_squeeze', squeeze = TRUE)

        expect_true(identical_tibble(x, parameter_d1_squeeze_squeezed))
    })

test_that(
    'read.gdx() coerces integer-like set elements to numeric',
    {
        x <- read.gdx(gdx, 'parameter_years')

        expect_identical(class(x[['set_years']]), 'numeric')
        expect_true(identical_tibble(x, parameter_years))
    })

test_that(
    'read.gdx() preserves +Inf, -Inf and NA special values',
    {
        x <- read.gdx(gdx, 'parameter_special')

        expect_true(identical_tibble(x, parameter_special))
    })

## variables ----
test_that(
    'read.gdx() reads scalar variables correctly',
    {
        x <- read.gdx(gdx, 'variable_d0')

        expect_equal(x, c('variable_d0' = variable_d0[['level']]))
    })

test_that(
    'read.gdx() reads one-dimensional variables correctly',
    {
        x <- read.gdx(gdx, 'variable_d1')

        expect_true(
            identical_tibble(
                x,
                variable_d1 %>%
                    select('set_d1_UPPER', 'value' = 'level')))

        x <- read.gdx(gdx, 'variable_d1',
                      fields = c('l', 'm', 'lo', 'up', 's'),
                      colNames = c('set_d1_UPPER', 'level', 'marginal',
                                   'lower', 'upper', 'scale'),
                      squeeze = FALSE)

        expect_true(identical_tibble(x, variable_d1))
    })

test_that(
    'read.gdx() reads two-dimensional variables correctly',
    {
        x <- read.gdx(gdx, 'variable_d2')

        expect_true(
            identical_tibble(x,
                             variable_d2 %>%
                                 select('set_d1_UPPER', 'set_d1_lower',
                                        'value' = 'level')))

        x <- read.gdx(gdx, 'variable_d2',
                      fields = c('l', 'm', 'lo', 'up', 's'),
                      colNames = c('set_d1_UPPER', 'set_d1_lower',
                                   'level', 'marginal', 'lower', 'upper',
                                   'scale'),
                      squeeze = FALSE)

        expect_true(identical_tibble(x, variable_d2))
    })

## equations ----
test_that(
    'read.gdx() reads scalar equations correctly',
    {
        x <- read.gdx(gdx, 'equation_d0')

        expect_identical(x, c('equation_d0' = equation_d0[['level']]))
    })

test_that(
    'read.gdx() reads one-dimensional equations correctly',
    {
        x <- read.gdx(gdx, 'equation_d1')

        expect_true(
            identical_tibble(
                x,
                equation_d1 %>%
                    select('set_d1_UPPER', 'value' = 'level')))

        x <- read.gdx(gdx, 'equation_d1',
                      fields = c('l', 'm', 'lo', 'up', 's'),
                      colNames = c('set_d1_UPPER', 'level', 'marginal',
                                   'lower', 'upper', 'scale'),
                      squeeze = FALSE)

        expect_true(identical_tibble(x, equation_d1))
    })

test_that(
    'read.gdx() reads two-dimensional equations correctly',
    {
        x <- read.gdx(gdx, 'equation_d2')

        expect_true(
            identical_tibble(x,
                             equation_d2 %>%
                                 select('set_d1_UPPER', 'set_d1_lower',
                                        'value' = 'level')))

        x <- read.gdx(gdx, 'equation_d2',
                      fields = c('l', 'm', 'lo', 'up', 's'),
                      colNames = c('set_d1_UPPER', 'set_d1_lower',
                                   'level', 'marginal', 'lower', 'upper',
                                   'scale'),
                      squeeze = FALSE)

        expect_true(identical_tibble(x, equation_d2))
    })
