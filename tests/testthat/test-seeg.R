test_that("uploads the data without error", {
  expect_no_error(load_seeg(dataset = "seeg_industry", 
                            raw_data = FALSE,
                            geo_level = "country",
                            language = "pt"), message="No success")
  expect_no_error(load_seeg(dataset = "seeg_farming", 
                            raw_data = FALSE,
                            geo_level = "state",
                            language = "eng"), message="No success")
})


