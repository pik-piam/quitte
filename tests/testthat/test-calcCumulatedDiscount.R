context('calcCumulatedDiscount()')

test_that(
  'Test calcCumulatedDiscount() results',
  {
    expect_equal(
      object = quitte_example_data %>%
        filter('r7552c_1p5C_Def-rem-5' == scenario,
               'World' == region,
               'Consumption' == variable) %>%
        calcCumulatedDiscount(discount = 0.03) %>%
        droplevels(),
      expected = data.frame(
        model    = 'REMIND',
        scenario = 'r7552c_1p5C_Def-rem-5',
        region   = 'World',
        variable = 'Consumption|aggregated',
        unit     = NA,
        period   = unique(remind_timesteps$period),
        value    = c(0, 141621.783393961, 281389.089154902, 424664.686579611,
                     569001.494200727, 713362.991162954, 856079.619654354,
                     995424.593907496, 1130136.50991505, 1259492.56034968,
                     1383488.53870724, 1500630.52493857, 1710955.47890831,
                     1897255.24316674, 2061703.82284864, 2202204.22445022,
                     2320200.47726592, 2471084.51930755, 2556397.07243914),
        stringsAsFactors = FALSE) %>%
        as.quitte()
    )
  })
