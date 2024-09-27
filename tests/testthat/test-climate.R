test_that("uploads the data without error", {
  expect_no_error(load_climate(dataset='precipitation',
                               time_period=2010,
                               legal_amazon_only = TRUE,
                               language = "pt"), message="No success")
  expect_no_error(load_climate(dataset='potential_evaporation',
                               time_period=2015,
                               legal_amazon_only = TRUE,
                               language = "eng"), message="No success")
})

