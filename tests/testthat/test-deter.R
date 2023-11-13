test_that("uploads the data without error", {
  expect_no_error(load_deter(dataset='deter_amz',
                             raw_data=TRUE), message="No success")
  expect_no_error(load_deter(dataset='deter_cerrado',
                             raw_data=TRUE), message="No success")
  expect_no_error(load_deter(dataset='deter_amz',
                             raw_data=FALSE), message="No success")
})


