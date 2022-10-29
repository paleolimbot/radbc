
test_that("error allocator works", {
  err <- radbc_allocate_error()
  expect_s3_class(err, "radbc_error")

  expect_output(expect_identical(print(err), err), "radbc_error")
  expect_output(expect_identical(str(err), err), "radbc_error")
  expect_identical(length(err), 3L)
  expect_identical(names(err), c("message", "vendor_code", "sqlstate"))
  expect_null(err$message)
  expect_identical(err$vendor_code, 0L)
  expect_identical(err$sqlstate, as.raw(c(0x00, 0x00, 0x00, 0x00, 0x00)))
})
