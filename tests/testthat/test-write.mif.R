context('write.mif()')

# write .mif files ----
test_that(
  'write .mif files',
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

# write .mif file with comment header ----
test_that(
  desc = 'write .mif file with comment header',
  code = {
    f <- tempfile()
    set.seed(0)
    quitte_example_data %>%
      slice_sample(n = 10) %>%
      write.mif(path = f, comment_header = c('foo: bar', 'fuzz: baz'))

    expect_equal(
      object = read_lines(f),
      expected = read_lines(system.file('extdata', 'comment_header.mif',
                                        package = 'quitte')))
  })
