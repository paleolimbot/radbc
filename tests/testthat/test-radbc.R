
test_that("can initialize and release a database", {
  db <- radbc_database_init(radbc_driver_void(), c("some_key" = "some_value"))
  expect_s3_class(db, "radbc_database")
  radbc_database_release(db)
  expect_error(radbc_database_release(db), "ADBC_STATUS_INVALID_STATE")
})

test_that("can initialize and release a connection", {
  db <- radbc_database_init(radbc_driver_void())
  con <- radbc_connection_init(db, c("some_key" = "some_value"))
  expect_s3_class(con, "radbc_connection")
  radbc_connection_release(con)
  expect_error(radbc_connection_release(con), "ADBC_STATUS_INVALID_STATE")
})
