context('read.quitte()')

test_that(
  'Test read.quitte() results',
  {
    expect_equal(
      object = full_join(
        read.quitte(
          system.file('extdata',
                      c('REMIND_generic_r7552c_1p5C_Def-rem-5.mif',
                        'REMIND_generic_r7552c_1p5C_UBA_Sust-rem-5.mif',
                        'REMIND_generic_r7552c_2C_Def-rem-5.mif',
                        'REMIND_generic_r7552c_2C_UBA_Sustlife-rem-5.mif',
                        'REMIND_generic_r7552c_REF_Def05-rem-5.mif'),
                      package = 'quitte')),
        quitte_example_data %>%
          rename(reference = value),
        c('model', 'scenario', 'region', 'variable', 'unit', 'period')
      ) %>%
        filter(value != reference) %>%
        nrow(),
      expected = 0
    )
  })
