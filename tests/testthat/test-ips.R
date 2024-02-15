test_that("uploads the data without error", {
  expect_no_error(load_ips(dataset = "deforest", raw_data = FALSE,
                           time_period = 2018, language = "pt"), message="No success")
  expect_no_error(load_ips(dataset = "life_quality", raw_data = FALSE,
                           time_period = 2013, language = "eng"), message="No success")
})
