context('gg_colour_hue()')

test_that(
    desc = 'Test if correct colours are returned',
    code = {
        expect_equal(object = gg_colour_hue(3),
                     expected = c('#F8766D', '#00BA38', '#619CFF'))

        expect_equal(object = gg_colour_hue(letters[1:3]),
                     expected = setNames(gg_colour_hue(3), letters[1:3]))
    }
)

test_that(
    desc = 'handling of illegal parameters',
    code = {
        expect_error(object = gg_colour_hue(gg_colour_hue),
                     regexp = 'n must be integer or character vector')
    }
)
