context('quitteSort()')

test_that('quitteSort works', {
  for (qe in list(quitte_example_dataAR6, quitte_example_data)) {
    qe <- quitteSort(as.quitte(qe, na.rm = TRUE))
    expect_identical(qe, quitteSort(qe))
    # move model column to the end
    qewrong <- qe %>%
      mutate(savemodel = model, model = NULL) %>%
      mutate(model = savemodel, savemodel = NULL)
    expect_false(identical(qe, qewrong))
    expect_identical(qe, quitteSort(qewrong))
    # move first region to the end
    firstregion <- qe$region[[1]]
    qewrong <- rbind(
                  filter(qe, region != firstregion),
                  filter(qe, region == firstregion)
                    )
    expect_false(identical(qe, qewrong))
    expect_identical(qe, quitteSort(qewrong))
  }
})
