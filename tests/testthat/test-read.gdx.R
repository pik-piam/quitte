# define tests ----
# all these tests a wrapped in a function so that they can be run with different
# options below
`test-read.gdx` <- function()
{
    gdx <- test_path('test_data/test.gdx')

    info <- paste('`quitte_force_gamstransfer` =',
                  options('quitte_force_gamstransfer'))

    identical_tibble <- function(x, y)
    {
        a <- all.equal(x, y)
        b <- all.equal(attributes(x)[sort(names(attributes(x)))],
                       attributes(y)[sort(names(attributes(y)))])

        if (!isTRUE(a))
        {
            return(a)
        }
        else if (!isTRUE(b))
        {
            return(b)
        }
        else
        {
            return(TRUE)
        }
    }

    ## sets ----
    test_that(
        'read.gdx() reads sets correctly',
        {
            x <- read.gdx(gdx, 'set_d1_lower')

            expect_true(tibble::is_tibble(x), info = info)
            expect_identical('foo', colnames(x), info = info)
            expect_identical(c('foo' = 'character'), sapply(x, class),
                             info = info)
            expect_equal(length(set_d1_lower), nrow(x), info = info)
        })

    test_that(
        'read.gdx() reads two-dimensional sets correctly',
        {
            x <- read.gdx(gdx, 'set_d2_alias')

            expect_true(tibble::is_tibble(x), info = info)

            expect_identical(c('set_d1_UPPER', 'set_d1_lower'), colnames(x),
                             info = info)

            expect_identical(
                sapply(x, class),
                c('set_d1_UPPER' = 'character', 'set_d1_lower' = 'character'),
                info = info)

            expect_equal(prod(lengths(list(set_d1_UPPER, set_d1_lower))),
                         nrow(x),
                         info = info)
        })

    test_that(
        'read.gdx() reads set aliases correctly',
        {
            x <- read.gdx(gdx, 'set_d2')
            y <- read.gdx(gdx, 'set_d2_alias')

            expect_identical(x, y, info = info)
        })

    ## parameters ----
    test_that(
        'read.gdx() reads scalars correctly',
        {
            x <- read.gdx(gdx, 'parameter_d0')

            expect_equal(object = x,
                         expected = c('parameter_d0' = parameter_d0),
                         info = info)
        })

    test_that(
        'read.gdx() reads parameters correctly',
        {
            x <- read.gdx(gdx, 'parameter_d1')

            expect_true(identical_tibble(x, parameter_d1), info = info)
        })

    test_that(
        'read.gdx() reads two-dimensional parameters correctly',
        {
            x <- read.gdx(gdx, 'parameter_d2')

            expect_true(identical_tibble(x, parameter_d2), info = info)
        })

    ## variables ----
    test_that(
        'read.gdx() reads scalar variables correctly',
        {
            x <- read.gdx(gdx, 'variable_d0')

            expect_equal(x,
                         c('variable_d0' = variable_d0[['level']]),
                         info = info)
        })

    test_that(
        'read.gdx() reads one-dimensional variables correctly',
        {
            x <- read.gdx(gdx, 'variable_d1')

            expect_true(
                identical_tibble(
                    x,
                    variable_d1 %>%
                        select('set_d1_UPPER', 'value' = 'level')),
                info = info)

            x <- read.gdx(gdx, 'variable_d1',
                          fields = c('l', 'm', 'lo', 'up', 's'),
                          colNames = c('set_d1_UPPER', 'level', 'marginal',
                                       'lower', 'upper', 'scale'),
                          squeeze = FALSE)

            expect_true(identical_tibble(x, variable_d1), info = info)
        })

    test_that(
        'read.gdx() reads two-dimensional variables correctly',
        {
            x <- read.gdx(gdx, 'variable_d2')

            expect_true(
                identical_tibble(x,
                                 variable_d2 %>%
                                     select('set_d1_UPPER', 'set_d1_lower',
                                            'value' = 'level')),
                info = info)

            x <- read.gdx(gdx, 'variable_d2',
                          fields = c('l', 'm', 'lo', 'up', 's'),
                          colNames = c('set_d1_UPPER', 'set_d1_lower',
                                       'level', 'marginal', 'lower', 'upper',
                                       'scale'),
                          squeeze = FALSE)

            expect_true(identical_tibble(x, variable_d2), info = info)
        })

    ## equations ----
    test_that(
        'read.gdx() reads scalar equations correctly',
        {
            x <- read.gdx(gdx, 'equation_d0')

            expect_identical(x,
                             c('equation_d0' = equation_d0[['level']]),
                             info = info)
        })

    test_that(
        'read.gdx() reads one-dimensional equations correctly',
        {
            x <- read.gdx(gdx, 'equation_d1')

            expect_true(
                identical_tibble(
                    x,
                    equation_d1 %>%
                        select('set_d1_UPPER', 'value' = 'level')),
                info = info)

            x <- read.gdx(gdx, 'equation_d1',
                          fields = c('l', 'm', 'lo', 'up', 's'),
                          colNames = c('set_d1_UPPER', 'level', 'marginal',
                                       'lower', 'upper', 'scale'),
                          squeeze = FALSE)

            expect_true(identical_tibble(x, equation_d1), info = info)
        })

    test_that(
        'read.gdx() reads two-dimensional equations correctly',
        {
            x <- read.gdx(gdx, 'equation_d2')

            expect_true(
                identical_tibble(x,
                                 equation_d2 %>%
                                     select('set_d1_UPPER', 'set_d1_lower',
                                            'value' = 'level')),
                info = info)

            x <- read.gdx(gdx, 'equation_d2',
                          fields = c('l', 'm', 'lo', 'up', 's'),
                          colNames = c('set_d1_UPPER', 'set_d1_lower',
                                       'level', 'marginal', 'lower', 'upper',
                                       'scale'),
                          squeeze = FALSE)

            expect_true(identical_tibble(x, equation_d2), info = info)
        })
}

# run tests ----
# set up test data to compare to
source(test_path('test_data/make_test_data.R'))

if (   'gdxrrw' %in% .packages(all.available = TRUE)
    && init_gdxrrw()) {
    withr::with_options(
        new = list('quitte_force_gamstransfer' = FALSE),
        `test-read.gdx`())
}

if ('gamstransfer' %in% .packages(all.available = TRUE)) {
    withr::with_options(
        new = list('quitte_force_gamstransfer' = TRUE),
        `test-read.gdx`())
}
