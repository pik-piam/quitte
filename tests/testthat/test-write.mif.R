context('write.mif()')

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

test_that(
  'write .mif file with comment header',
  {
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

test_that(
  'write .mif file passing empty data',
  {
    f <- tempfile()
    expect_warning(write.mif(filter(quitte_example_data, model == "x"), f),
                   "Writing empty data frame")
  })

test_that(
  'write IAMC format',
  {
    f <- tempfile(fileext = 'csv')
    write.IAMCcsv(quitte_example_data, f)
    expect_equal(object = read.snapshot(f), expected = quitte_example_data)
  })
