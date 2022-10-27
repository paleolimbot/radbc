
test_that("error allocator works", {
  err <- radbc_allocate_error()
  expect_s3_class(err, "radbc_error")
})
