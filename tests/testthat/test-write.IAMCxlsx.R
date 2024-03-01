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
    # expect warning if incomplete data is read
    f2 <- paste0(tempfile(), ".xlsx")
    quitte_incomplete <- quitte_example_data %>% select(- model)
    quitte_incomplete <- pivot_wider(quitte_incomplete, names_from = 'period', values_from = 'value')
    writexl::write_xlsx(list("data" = quitte_incomplete), f2)
    expect_warning(object = read.quitte(f2), "misses default columns")
  })

test_that(
  'write empty data to a .xlsx file',
  {
    f3 <- paste0(tempfile(), ".xlsx")
    expect_warning(write.IAMCxlsx(filter(quitte_example_data, model == "x"), f3),
                   "Writing empty data frame")
  })
