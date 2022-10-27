
test_that("can initialize and release a database", {
  db <- radbc_database_init(radbc_driver_void())
  expect_s3_class(db, "radbc_database")
  radbc_database_release(db)
  expect_error(radbc_database_release(db), "ADBC_STATUS_INVALID_STATE")
})
