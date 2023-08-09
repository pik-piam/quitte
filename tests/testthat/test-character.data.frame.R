context("character.data.frame()")

# get some test data
df <- read.quitte(
    system.file('extdata', 'REMIND_generic_r7552c_1p5C_Def-rem-5.mif',
                package = 'quitte'))

cdf <- character.data.frame(df)

test_that(
    'Test correct attributes on columns',
    {
        # identical number of columns
        expect_equal(
            object   = ncol(cdf),
            expected = ncol(df))

        # identical number of rows
        expect_equal(
            object   = nrow(cdf),
            expected = nrow(df))

        # identical object attributes
        expect_true(
            object = setequal(names(attributes(df)), names(attributes(cdf))))

        # no 'dim' or 'dimnames' attributes
        expect_false(any(sapply(cdf, function(x) {
            any(c('dim', 'dimnames') %in% names(attributes(x)))
            })))
    }
)
