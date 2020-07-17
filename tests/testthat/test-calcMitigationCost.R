context('calcMitigationCost()')

test_that(
  'Test calcMitigationCost() results',
  {
    expect_equal(
      object = calcMitigationCost(data = quitte_example_data,
                                  scenBau = 'r7552c_REF_Def05-rem-5',
                                  scenPol = 'r7552c_1p5C_UBA_Sust-rem-5') %>%
        droplevels(),
      expected = quitte_example_data %>%
        select(model, scenario, region) %>%
        filter('r7552c_1p5C_UBA_Sust-rem-5' == scenario) %>%
        distinct(region, .keep_all = TRUE) %>%
        droplevels() %>%
        mutate(variable = factor('Mitigation cost'),
               unit     = factor('pp'),
               period   = NA_integer_,
               value    = c(  1.41858860121024,   1.10579004988017,
                             -0.397110943883032,  1.27344789025818,
                             -0.21268178803817,   1.12313193047381,
                              4.05412414295081,   0.349536410032298,
                              1.31246099888181,   7.15122407875714,
                             -0.0313855679745607, 0.615055315381734)) %>%
        as.data.frame() %>%
        as.quitte()
    )
  })
