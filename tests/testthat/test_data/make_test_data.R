library(dplyr)
library(tidyr)

# sets ----
set_d1_lower <- letters[23:26]
set_d1_UPPER <- LETTERS[1:3]
set_d2 <- expand_grid(set_d1_UPPER, set_d1_lower)

set_d2_identical <- tibble(set_d1_lower, set_d1_lower,
                           .name_repair = function(names) {
                               make.names(names, unique = TRUE) })

# parameters ----
parameter_d0 <- 13

parameter_d1 <- tibble(set_d1_UPPER) %>%
    mutate(value = 1:n())

parameter_d2 <- set_d2 %>%
    mutate(
        value = as.integer((Vectorize(charToRaw))(set_d1_UPPER)) * 10000
              + as.integer((Vectorize(charToRaw))(set_d1_lower)))

parameter_d2_0 <- parameter_d2 %>%
    filter(FALSE)

# variables ----
variable_d0 <- data.frame('level'    =  13,
                          'marginal' =  -1,
                          'lower'    =   0,
                          'upper'    = Inf,
                          'scale'    =  10)

variable_d1 <- expand_grid(set_d1_UPPER,
                           variable_d0 %>%
                               pivot_longer(everything())) %>%
    mutate(value = value
                 * ( as.integer((Vectorize(charToRaw))(set_d1_UPPER))
                   - as.integer(charToRaw(set_d1_UPPER[1]))
                   + 1)) %>%
    pivot_wider()

variable_d2 <- expand_grid(set_d2,
                           variable_d0 %>%
                               pivot_longer(everything())) %>%
    mutate(value = value
                 * ( ( as.integer((Vectorize(charToRaw))(set_d1_UPPER))
                     - as.integer(charToRaw(set_d1_UPPER[1]))
                     + 1
                     )
                   * 10000
                   + ( as.integer((Vectorize(charToRaw))(set_d1_lower))
                     - as.integer(charToRaw(set_d1_lower[1]))
                     + 1
                     )
                   )) %>%
    pivot_wider()

# equations ----
equation_d0 <- variable_d0
equation_d1 <- variable_d1
equation_d2 <- variable_d2
