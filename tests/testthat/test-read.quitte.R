test_that(
    'read.quitte() reads .mif files correctly',
    {
        expect_equal(object = read.quitte(
            system.file('extdata',
                        c('REMIND_generic_r7552c_1p5C_Def-rem-5.mif',
                          'REMIND_generic_r7552c_1p5C_UBA_Sust-rem-5.mif',
                          'REMIND_generic_r7552c_2C_Def-rem-5.mif',
                          'REMIND_generic_r7552c_2C_UBA_Sustlife-rem-5.mif',
                          'REMIND_generic_r7552c_REF_Def05-rem-5.mif'),
                        package = 'quitte')) %>%
            getElement("value"),
                   expected = quitte_example_data %>%
            getElement("value"))
    })

test_that(
    'extra columns are included in duplicate checks',
    {
        expect_warning(object = read.quitte(system.file('extdata',
                                                        'extra_column.mif',
                                                        package = 'quitte')),
                       regexp = NA)
    })

test_that(
    'warning raised in case of duplicates',
    {
        expect_warning(object = read.quitte(
            file = rep(system.file('extdata', 'extra_column.mif',
                                   package = 'quitte'), 2)),
            regexp = "Duplicated data found for")
    })

test_that(
    'read .mif file with comment header',
    {
        d <- quitte_example_data %>%
            slice_sample(n = 10)

        comment_header <- c('foo: bar', 'fuzz: baz')

        write.mif(d, f1 <- tempfile(), comment_header = comment_header)
        write.mif(d, f2 <- tempfile())

        d1 <- read.quitte(f1)
        d2 <- read.quitte(f2)
        attr(d2, 'comment_header') <- comment_header

        expect_equal(object = d1, expected = d2)
    })

test_that(
    'read.quitte() reports problems',
    {
        # insert parsing problem into temporary file
        x <- read_lines(system.file('extdata', 'extra_column.mif',
                                    package = 'quitte', mustWork = TRUE))
        x[[length(x)]] <- sub('[0-9]+$', 'Inf', x[[length(x)]])
        tmp <- tempfile('read_delim_problem', tempdir(), '.mif')
        write_lines(x, tmp)

        # warns about problems
        expect_warning(object = x <- read.quitte(tmp),
                       regexp = 'One or more parsing issues')

        # returns a data frame as the `problems` attribute
        expect_s3_class(object = problems(x), class = 'data.frame')
        # data frame has more then one row
        expect_gt(object = nrow(problems(x)), expected = 0)
        # data frame has a `file` column
        expect_contains(object = colnames(problems(x)), expected = 'file')

        unlink(tmp)
    })

test_that(
    'read.quitte(filter.function) filters correctly',
    {
        df_diff <- function(lhs, rhs, value = value) {
            value <- sub('^"(.*)"$', '\\1', deparse(substitute(value)))
            lhs.value <- paste0('lhs.', value)
            rhs.value <- paste0('rhs.', value)

            tmp <- full_join(
                lhs %>%
                    rename(!!sym(lhs.value) := all_of(value)),

                rhs %>%
                    rename(!!sym(rhs.value) := all_of(value))
            ) %>%
                filter(!!sym(lhs.value) != !!sym(rhs.value))

            if (0 == nrow(tmp)) {
                return(NULL)
            }
            else {
                return(tmp)
            }
        }

        lhs <- read.quitte(
            file = system.file(
                'extdata',
                c('REMIND_generic_r7552c_1p5C_Def-rem-5.mif',
                  'REMIND_generic_r7552c_1p5C_UBA_Sust-rem-5.mif',
                  'REMIND_generic_r7552c_2C_Def-rem-5.mif',
                  'REMIND_generic_r7552c_2C_UBA_Sustlife-rem-5.mif',
                  'REMIND_generic_r7552c_REF_Def05-rem-5.mif'),
                package = 'quitte')) %>%
            filter(!grepl('^PE\\|', variable))

        rhs <- read.quitte(
            file = system.file(
                'extdata',
                c('REMIND_generic_r7552c_1p5C_Def-rem-5.mif',
                  'REMIND_generic_r7552c_1p5C_UBA_Sust-rem-5.mif',
                  'REMIND_generic_r7552c_2C_Def-rem-5.mif',
                  'REMIND_generic_r7552c_2C_UBA_Sustlife-rem-5.mif',
                  'REMIND_generic_r7552c_REF_Def05-rem-5.mif'),
                package = 'quitte'),
            filter.function = function(x) {
                x %>%
                    filter(!grepl('^PE\\|', variable))
            })

          expect_null(df_diff(lhs, rhs))
    })

test_that(
    'read.quitte() reads IAMC .csv files',
    {
        expect_contains(
            object = read.quitte(
                file = system.file(
                    'extdata', 'ar6_climate_assessment_SSP2-PkBudg1000-AMT.csv',
                    package = 'quitte'),
                sep = ',') |>
            class(),
            expected = 'quitte')
    })
