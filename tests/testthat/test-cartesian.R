test_that(
    'cartesian(...) generates correct combinations',
    {
        expect_equal(
            object = cartesian(1:2),
            expected = c('1', '2')
        )

        expect_equal(
            object = cartesian(1:2, 3:5),
            expected = c('1.3', '1.4', '1.5', '2.3', '2.4', '2.5')
        )

        expect_equal(
            object = cartesian(1:2, 3:6, 7:9),
            expected = c('1.3.7', '1.3.8', '1.3.9', '1.4.7', '1.4.8', '1.4.9',
                         '1.5.7', '1.5.8', '1.5.9', '1.6.7', '1.6.8', '1.6.9',
                         '2.3.7', '2.3.8', '2.3.9', '2.4.7', '2.4.8', '2.4.9',
                         '2.5.7', '2.5.8', '2.5.9', '2.6.7', '2.6.8', '2.6.9'))
    })

test_that(
    'cartesian() returns NULL',
    {
        expect_null(object = cartesian())
    })
