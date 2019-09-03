#' @name remind_timesteps
#'
#' @title REMIND time steps
#'
#' @description A data frame containing the weights with which years contribute
#' to specific periods.
#'
#' @docType data
#'
#' @usage remind_timesteps
#'
#' @author Michaja Pehl

NULL

# library(tidyverse)
#
# remind_timesteps <- tibble(
#     period = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150),
#     length = c(rep(5, 11), 7.5, rep(10, 4), 15, 20, 27)) %>%
#     mutate(
#         start = first(period)
#               + cumsum(c(0, head(length, -1)))
#               - (first(length) - 1) / 2,
#         end   = start + length - 1) %>%
#     mutate(year = NA_integer_) %>%
#     complete(nesting(period, length, start, end),
#              year = as.integer(floor(min(start)):ceiling(max(end)))) %>%
#     group_by(period) %>%
#     mutate(contained = year %in% floor(min(start)):ceiling(max(end))) %>%
#     ungroup() %>%
#     filter(contained) %>%
#     mutate(weight = (start - year) * (year <= start)
#                   + as.integer(start <= year & year <= end)
#                   + (year - end) * (year >= end)) %>%
#     select(period, year, weight)
#
# save(remind_timesteps, file = './data/remind_timesteps.rda')
