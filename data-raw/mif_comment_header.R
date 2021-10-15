# Write some test data for .mif comment headers
library(tidyverse)
library(quitte)

f <- './inst/extdata/comment_header.mif'

set.seed(0)
quitte_example_data %>%
    slice_sample(n = 10) %>%
    write.mif(path = f, comment_header = c('foo: bar', 'fuzz: baz'))
