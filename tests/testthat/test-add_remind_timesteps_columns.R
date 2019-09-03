context('add_remind_timesteps_columns ranges')

test_that(
    'remind_timesteps and add_remind_timesteps_columns are correct',

    {testthat::expect_true(remind_timesteps %>%
                               group_by(period) %>%
                               summarise(length = sum(weight)) %>%
                               ungroup() %>%
                               add_remind_timesteps_columns() %>%
                               mutate(start = xpos - width / 2,
                                      end   = xpos + width / 2) %>%
                               mutate(correct = length == width
                                      & ifelse(first(period) == period, TRUE,
                                               start == lag(end))) %>%
                               getElement('correct') %>%
                               all())
    })
