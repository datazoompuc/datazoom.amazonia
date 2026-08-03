# This file is part of the standard testthat setup for R packages.
# It is run by R CMD check but not by devtools::test().

library(testthat)
library(datazoom.amazonia)

test_check("datazoom.amazonia")
