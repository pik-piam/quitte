context('madrat_mule()')

test_that(
    desc = 'madrat_mule() returns identical tibbles',
    code = expect_identical(
        object = madrat_mule(madrat_mule(quitte_example_data)),
        expected = quitte_example_data)
)
