context('write.IAMCxlsx()')

# write .xlsx files ----
test_that(
  'write .xlsx files',
  {
    f <- paste0(tempfile(), ".xlsx")
    write.IAMCxlsx(quitte_example_data, f)

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
    # check append function
    quitte_new_model <- quitte_example_data
    quitte_new_model$model <- "REMIND-MAgPIE"
    quitte_new_model$value[quitte_new_model$region == "AFR" & quitte_new_model$period == 2020] <- NA
    write.IAMCxlsx(quitte_new_model, f, append = TRUE)
    expect_equal(
        object = read.quitte(f) %>%
          character.data.frame() %>%
          arrange(!!sym('model'), !!sym('scenario'), !!sym('region'),
                  !!sym('variable'), !!sym('unit'), !!sym('period')),
      expected = rbind(quitte_example_data, quitte_new_model) %>%
        character.data.frame() %>%
        arrange(!!sym('model'), !!sym('scenario'), !!sym('region'),
                !!sym('variable'), !!sym('unit'), !!sym('period'))
      )
  })
