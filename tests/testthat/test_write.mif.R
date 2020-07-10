context('write.mif()')

test_that(
  'Test writing of .mif files',
  {
    f <- tempfile()
    quitte_example_data %>%
      write.mif(f)

    expect_equal(
        object = read.quitte(f) %>%
          character.data.frame() %>%
          arrange(!!sym('model'), !!sym('scenario'), !!sym('region'),
                  !!sym('variable'), !!sym('unit'), !!sym('period')),
      expected = quitte_example_data %>%
        character.data.frame() %>%
        arrange(!!sym('model'), !!sym('scenario'), !!sym('region'),
                !!sym('variable'), !!sym('unit'), !!sym('period'))
      )
  })
