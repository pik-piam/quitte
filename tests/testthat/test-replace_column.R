context('replace_column()')

test_that(
  'Test replace_column() results',
  {
    # ---- simple example with matching old and match column names ----
    model_data <- data.frame(
      model  = c('Model1', '2ndModel', 'Model Three'),
      region = c('Region 1', 'Region 2', 'Region 1'),
      value  = 1:3,
      stringsAsFactors = FALSE)

    mask <- data.frame(
      model  = c('Model1', '2ndModel', 'Model Three', 'fourth Model'),
      clear_name = paste('Model', 1:4),
      stringsAsFactors = FALSE)

    expect_equal(
      object = replace_column(model_data, mask, model, clear_name),
      expected = data.frame(model  = paste('Model', 1:3),
                            region = paste('Region', c(1, 2, 1)),
                            value  = 1:3,
      stringsAsFactors = FALSE)
    )

    # ---- mismatched column names ----
    model_data <- data.frame(
      model  = c('Model1', '2ndModel', 'Model Three', 'fourth Model'),
      region = c('Region 1', 'Region 2', 'Region 1', 'Region 2'),
      value  = 1:4,
      stringsAsFactors = FALSE)

    mask <- data.frame(
      ugly_name  = c('Model1', '2ndModel', 'Model Three'),
      clear_name = paste('Model', 1:3),
      stringsAsFactors = FALSE)

    expect_equal(
      object = replace_column(model_data, mask, model = ugly_name, clear_name),
      expected = data.frame(
        model   = c(paste('Model', 1:3), NA),
        region  = paste('Region', c(1, 2, 1, 2)),
        value   = 1:4,
      stringsAsFactors = FALSE)
    )

    # ---- SE example ----
    expect_equal(
      object = replace_column_(model_data, mask, 'model', 'ugly_name',
                               'clear_name'),
      expected = data.frame(
        model   = c(paste('Model', 1:3), NA),
        region  = paste('Region', c(1, 2, 1, 2)),
        value   = 1:4,
      stringsAsFactors = FALSE)
    )

    # ---- dropping the extra entries in model ----
    expect_equal(
      object = replace_column(model_data, mask, model = ugly_name, clear_name,
                              drop.extra = TRUE),
      expected = data.frame(model  = paste('Model', 1:3),
                            region = paste('Region', c(1, 2, 1)),
                            value  = 1:3,
      stringsAsFactors = FALSE)
    )

    # ---- also works on quitte objects ----
    quitte <- tibble(model    = c('Model1', '2ndModel'),
                     scenario = 'Scenario',
                     region   = 'Region',
                     variable = 'Variable',
                     unit     = 'Unit',
                     period   = 2010,
                     value    = 1:2) %>%
      as.quitte()

    expect_equal(
      object = replace_column(quitte, mask, model = ugly_name, clear_name),
      expected = tibble(model    = paste('Model', 1:2),
                        scenario = 'Scenario',
                        region   = 'Region',
                        variable = 'Variable',
                        unit     = 'Unit',
                        period   = 2010,
                        value    = 1:2) %>%
        as.quitte()
    )
  })

test_that(
  desc = 'replace_column() warning for ambiguous mask',
  code = {
    model_data <- data.frame(
      model  = c('Model1', '2ndModel', 'Model Three'),
      region = c('Region 1', 'Region 2', 'Region 3'),
      value  = 1:3,
      stringsAsFactors = FALSE)

    mask <- data.frame(
      model  = c('Model1', '2ndModel', 'Model Three')[c(1, 2, 3, 3, 2)],
      clear_name = paste('Model', 1:5),
      stringsAsFactors = FALSE)

    expect_warning(
      object = replace_column(model_data, mask, model, clear_name),
      regexp = 'No unambiguous match for.*')

    expect_failure(
      expect_warning(
        object = replace_column(model_data, mask, model, clear_name,
                                ignore.ambiguous.match = TRUE),
        regexp = 'No unambiguous match for.*')
    )

    mask <- data.frame(
      model  = c('Model1', '2ndModel', 'Model Three')[c(1, 2, 3)],
      clear_name = paste('Model', 1:3),
      stringsAsFactors = FALSE)

    expect_failure(
      expect_warning(
        object = replace_column(model_data, mask, model, clear_name),
        regexp = 'No unambiguous match for.*')
    )
  }
)
