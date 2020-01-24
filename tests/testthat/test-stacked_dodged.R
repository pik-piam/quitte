context('add_stacked_dodged_xpos()')

# ---- parameters ----
test_that(
    desc = 'add_stacked_dodged_xpos() parameters',
    code = {
        # first parameter needs to be a data frame
        expect_error(object = add_stacked_dodged_xpos(0, 1:2),
                     regexp = 'requires a data frame$')

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

test_that(
    desc = 'calc_stacked_dodged_xlabels() position calculation',
    code = {
        test.data <- crossing(a = factorise(c('Z', 'A')),
                              b = factorise(c('x', 'c')))

        expect_equal(
            object = calc_stacked_dodged_xlabels(test.data, c('a', 'b')),
            expected = c(Z = 1.5, A = 4.5))

        expect_equal(
            object = calc_stacked_dodged_xlabels(test.data, c('a', 'b'),
                                                 gap = 2),
            expected = c(Z = 1.5, A = 5.5))

        test.data <- crossing(a = LETTERS[1:3],
                              b = letters[1:2])

        expect_equal(
            object = calc_stacked_dodged_xlabels(test.data, c('a', 'b')),
            expected = c(A = 1.5, B = 4.5, C = 7.5))

        expect_equal(
            object = calc_stacked_dodged_xlabels(
                crossing(a = factorise(c('left', 'center', 'right')),
                         b = factorise(c('top', 'middle', 'bottom')),
                         c = letters[1:4],
                         d = LETTERS[25:26]),
                c('c', 'a')),
            expected = c(a = 2, b = 6, c = 10, d = 14))

        expect_equal(
            object = calc_stacked_dodged_xlabels(
                crossing(a = factorise(c('left', 'center', 'right')),
                         b = factorise(c('top', 'middle', 'bottom')),
                         c = letters[1:4],
                         d = LETTERS[25:26]),
                c('c', 'a'),
                gap = 1/3),
            expected = c(a = 2, b = 16/3, c = 26/3, d = 12))

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

# ---- dodge/stack numericals ----
test_that(
    desc = 'add_stack_dodge_xpos() numerical columns',
    code = {
        expect_equal(
            object = add_stacked_dodged_xpos(
                data = crossing(a = c('a', 'b'),
                                b = c('A', 'B'),
                                t = c(1, 2)),
                list(xpos = c('t', 'b'))),
            expected = add_stacked_dodged_xpos(
                data = crossing(a = c('a', 'b'),
                                b = c('A', 'B'),
                                t = c('1', '2')),
                list(xpos = c('t', 'b'))) %>%
                mutate(t = as.numeric(t)))
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
                gap = 1/2),
            expected = c('1' = 1.5, '2' = 4))

        expect_equal(
            object = calc_stacked_dodged_xlabels(
                data = crossing(a = 1:3, b = 1:2),
                c('a', 'b'),
                gap = 1/2),
            expected = c('1' = 1.5, '2' = 4, '3' = 6.5))

        expect_equal(
            object = calc_stacked_dodged_xlabels(
                data = crossing(a = 1:2, b = 1:2),
                c('a', 'b'),
                gap = 2),
            expected = c('1' = 1.5, '2' = 5.5))
    }
)

context('ggplot_bar_stacked_dodge()')

# ---- plot parameters ----
test_that(
    desc = 'parameters',
    code = {
        expect_error(
            object = ggplot_bar_stacked_dodged(NULL, aes()),
            regexp = 'requires a data frame')

        expect_error(
            object = ggplot_bar_stacked_dodged(data.frame(), NULL),
            regexp = paste0('ggplot_bar_stacked_dodged requires the following',
                            ' missing aesthetics: x, y, fill, dodge'))

        expect_error(
            object = ggplot_bar_stacked_dodged(data.frame(),
                                               ggplot2::aes(fill = 0, x = 0)),
            regexp = paste0('ggplot_bar_stacked_dodged requires the following',
                            ' missing aesthetics: y, dodge'))

        expect_error(
            object = ggplot_bar_stacked_dodged(
                data.frame(x = 0, y = 0, fill = 0, dodge = 0),
                ggplot2::aes(x = x, y = value, fill = llif, dodge = dodge)),
            regexp = 'missing columns? .*')
    }
)
