
test_that("void driver init function works", {
  expect_s3_class(radbc_driver_void(), "radbc_driver")
  expect_s3_class(radbc_driver_void()$driver_init_func, "radbc_driver_init_func")
})
