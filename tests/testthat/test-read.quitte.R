context('read.quitte()')

# read.quitte() results ----
test_that(
  'read.quitte() results',
  {
    object = read.quitte(
      system.file('extdata',
                  c('REMIND_generic_r7552c_1p5C_Def-rem-5.mif',
                    'REMIND_generic_r7552c_1p5C_UBA_Sust-rem-5.mif',
                    'REMIND_generic_r7552c_2C_Def-rem-5.mif',
                    'REMIND_generic_r7552c_2C_UBA_Sustlife-rem-5.mif',
                    'REMIND_generic_r7552c_REF_Def05-rem-5.mif'),
                  package = 'quitte')) %>%
      getElement("value")

    expected = quitte_example_data %>%
      getElement("value")
    expect_equal(object,
      expected = expected
    )
  })

# extra columns are included in duplicate checks ----
test_that(
  'extra columns are included in duplicate checks',
  {
    expect_warning(object = read.quitte(system.file('extdata',
                                                    'extra_column.mif',
                                                    package = 'quitte')),
                   regexp = NA)
  })

# read .mif file with comment header ----
test_that(
  desc = 'read .mif file with comment header',
  code = {
    d <- quitte_example_data %>%
      slice_sample(n = 10)

    comment_header <- c('foo: bar', 'fuzz: baz')

    write.mif(d, f1 <- tempfile(), comment_header = comment_header)
    write.mif(d, f2 <- tempfile())

    d1 <- read.quitte(f1)
    d2 <- read.quitte(f2)
    attr(d2, 'comment_header') <- comment_header

    expect_equal(
      object = d1,
      expected = d2)
  })
