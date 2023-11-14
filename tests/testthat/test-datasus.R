test_that("uploads the data without error", {
  expect_no_error(load_datasus(dataset = "datasus_sim_do",
                               time_period = 2010,
                               states = c("AM", "PA"),
                               raw_data = FALSE,
                               keep_all = FALSE,
                               language = "pt"), message="No success")
})
