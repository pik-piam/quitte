context('quitteSort()')

test_that('quitteSort works', {
  for (qe in list(quitte_example_dataAR6, quitte_example_data)) {
    qe <- droplevels(as.quitte(qe, na.rm = TRUE))
    qes <- droplevels(quitteSort(qe))
    for (type in colnames(qe)) {
      expect_equal(sort(unique_or_levels(qe[[type]])), sort(unique_or_levels(qes[[type]])))
    }
    expect_identical(nrow(qe), nrow(qes))
    # move model column to the end
    qewrong <- relocate(qes, model, .after = value)
    expect_false(identical(qes, qewrong))
    expect_identical(qes, quitteSort(qewrong))
    # move first region to the end
    qewrong <- bind_rows(
      qes %>% filter(first(region) != region),
      qes %>% filter(first(region) == region)
    )
    expect_false(identical(qes, qewrong))
    expect_identical(qes, quitteSort(qewrong))
  }
})
