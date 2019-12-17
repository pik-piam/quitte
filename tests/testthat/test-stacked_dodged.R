context('add_stacked_dodged_xpos()')

# ---- parameters ----
test_that(
    desc = 'add_stacked_dodged_xpos() parameters',
    code = {
        # first parameter needs to be a data frame
        expect_error(object = add_stacked_dodged_xpos(0, 1:2),
                     regexp = 'requires a data.frame$')

        # unspecified parameter needs to be vector of length no less than two
        expect_error(object = add_stacked_dodged_xpos(data.frame(), 0),
                     regexp = 'requires two columns to be combined$')

        # columns to be combined must be present in data
        expect_error(object = add_stacked_dodged_xpos(data.frame(a = 0, b = 0),
                                                      c('A', 'b')),
                     rexexp = 'column.*not found$')
    }
)

# ---- position calculation ----
test_that(
    desc = 'add_stacked_dodged_xpos() position calculation',
    code = {
        test.data <- crossing(a = factorise(c('Z', 'A')),
                              b = factorise(c('x', 'c')))

        expect_equal(
            object = add_stacked_dodged_xpos(data = test.data,
                                             xpos = c('a', 'b')),
            expected = test.data %>%
                mutate(xpos = c(1, 2, 4, 5)))

        expect_equal(
            object = add_stacked_dodged_xpos(data = test.data,
                                             xpos = c('a', 'b'),
                                             width = 2),
            expected = test.data %>%
                mutate(xpos = c(1, 3, 6, 8)))

        expect_equal(
            object = add_stacked_dodged_xpos(data = test.data,
                                             xpos = c('a', 'b'),
                                             gap = 2),
            expected = test.data %>%
                mutate(xpos = c(1, 2, 5, 6)))

        test.data <- crossing(a = LETTERS[1:3],
                              b = letters[1:2])

        expect_equal(
            object = add_stacked_dodged_xpos(test.data, c('a', 'b')),
            expected = test.data %>%
                mutate(xpos = c(1, 2, 4, 5, 7, 8)))
    }
)

# ---- column preservation ----
test_that(
    desc = ' add_stacked_dodge_xpos() column preservation',
    code = {
        test.data <- crossing(a = factorise(c('Z', 'A')),
                              b = factorise(c('x', 'c'))) %>%
            mutate(val = 1)

        expect_equal(
            object = add_stacked_dodged_xpos(data = test.data, c('a', 'b')),
            expected = test.data %>%
                mutate(xpos = c(1, 2, 4, 5)))

        test.data <- crossing(a = c('Z', 'A'),
                              b = c('x', 'c')) %>%
            arrange(desc(a), desc(b)) %>%
            mutate(val = 1)

        expect_equal(
            object = add_stacked_dodged_xpos(data = test.data, c('a', 'b')),
            expected = test.data %>%
                mutate(xpos = c(1, 2, 4, 5)))
    }
)

# ---- column name clashes ----
test_that(
    desc = 'add_stack_dodge_xpos() column name clashes',
    code = {
        expect_error(
            object = add_stacked_dodged_xpos(
                data = crossing(a = factorise(c('Z', 'A')),
                                b = factorise(c('x', 'c'))) %>%
                    mutate(xpos = 1),
                c('a', 'b')),
            regexp = 'Default column name "xpos" already in use.')

        expect_error(
            object = add_stacked_dodged_xpos(
                data = crossing(a = factorise(c('Z', 'A')),
                                b = factorise(c('x', 'c'))) %>%
                    mutate(x = 1),
                x = c('a', 'b')),
            regexp = 'Column name "x" already in use.')
    }
)

context('calc_stacked_dodged_xlabels()')

# ---- label positions ----
test_that(
    desc = 'label positions',
    code = {
        expect_equal(
            object = calc_stacked_dodged_xlabels(
                data = crossing(a = 1:2, b = 1:2),
                c('a', 'b')),
            expected = c('1' = 1.5, '2' = 4.5))

        expect_equal(
            object = calc_stacked_dodged_xlabels(
                data = crossing(a = 1:2, b = 1:2),
                c('a', 'b'),
                width = 2),
            expected = c('1' = 2, '2' = 7))

        expect_equal(
            object = calc_stacked_dodged_xlabels(
                data = crossing(a = 1:3, b = 1:2),
                c('a', 'b'),
                width = 2),
            expected = c('1' = 2, '2' = 7, '3' = 12))

        expect_equal(
            object = calc_stacked_dodged_xlabels(
                data = crossing(a = 1:2, b = 1:2),
                c('a', 'b'),
                gap = 2),
            expected = c('1' = 1.5, '2' = 5.5))
    }
)
