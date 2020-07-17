context('aggregate_map()')

test_that(
  'Test aggregate_map results',
  {
    data <- inline.data.frame(c(
      "model;    scenario;   region;   variable;           unit;         period;   value",
      "REMIND;   Baseline;   USA;      GDP per Capita|MER; US$2005/yr;   2010;     40000",
      "REMIND;   Baseline;   USA;      GDP per Capita|MER; US$2005/yr;   2020;     50000",
      "REMIND;   Baseline;   USA;      Population;         million;      2010;     300",
      "REMIND;   Baseline;   USA;      Population;         million;      2020;     350",
      "REMIND;   Baseline;   CHN;      GDP per Capita|MER; US$2005/yr;   2010;     7000",
      "REMIND;   Baseline;   CHN;      GDP per Capita|MER; US$2005/yr;   2020;     8000",
      "REMIND;   Baseline;   CHN;      Population;         million;      2010;     1300",
      "REMIND;   Baseline;   CHN;      Population;         million;      2020;     1400"))

    mapping = inline.data.frame(c(
      "region;      New_region",
      "USA;         GLO",
      "CHN;         GLO"
    ))

    mapping2 = inline.data.frame(c(
      "Item      ;         Item_new",
      "Population;         Urban Population ",
      "Population;         Rural Population"
    ))

    weights = inline.data.frame(c(
      "region; itemI           ;   weight",
      "USA   ; Urban Population;      0.5",
      "USA   ; Rural Population;      0.2",
      "CHN   ; Urban Population;      2",
      "CHN   ; Rural Population;      1"
    ))

    expect_equal(
      object = aggregate_map(data,mapping, by = "region",
                             subset2agg = c("Population")),
      expected = tibble(
        model    = 'REMIND',
        scenario = 'Baseline',
        variable = 'Population',
        unit     = 'million',
        period   = as.integer(c(2010, 2020)),
        region   = 'GLO',
        value    = c(1600, 1750)))

    expect_equal(
      object = aggregate_map(data,mapping, by = "region",
                             subset2agg = "GDP per Capita|MER",
                             weights = "Population"),
      expected = tibble(
        model    = 'REMIND',
        scenario = 'Baseline',
        variable = 'GDP per Capita|MER',
        unit     = 'US$2005/yr',
        period   = as.integer(c(2010, 2020)),
        region   = 'GLO',
        value    = c(13187.5, 16400)))

    expect_equal(
      object = aggregate_map(data,mapping2, by = c("variable" = "Item"),
                             subset2agg = c("Population"),weights = weights,
                             weight_val_col = "weight",
                             weight_item_col = "itemI"),
      expected = tibble(
        model    = 'REMIND',
        scenario = 'Baseline',
        region   = rep(c('USA', 'CHN'), each = 4),
        unit     = 'million',
        period   = rep(rep(as.integer(c(2010, 2020)), each = 2), 2),
        value    = c(214.28571, 85.71429, 250, 100, 866.66667, 433.33333,
                     933.33333, 466.66667),
        variable = paste(rep(c('Urban', 'Rural'), 4), 'Population')))
  })
