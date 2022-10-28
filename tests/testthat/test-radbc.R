
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

test_that("connection methods work for the void driver", {
  db <- radbc_database_init(radbc_driver_void())
  con <- radbc_connection_init(db)

  expect_error(
    radbc_connection_get_info(con, integer()),
    "ADBC_STATUS_NOT_IMPLEMENTED"
  )

  expect_error(
    radbc_connection_get_objects(
      con, 0,
      "catalog", "db_schema",
      "table_name", "table_type", "column_name"
    ),
    "ADBC_STATUS_NOT_IMPLEMENTED"
  )

  expect_error(
    radbc_connection_get_table_schema(
      con,
      "catalog", "db_schema", "table_name"
    ),
    "ADBC_STATUS_NOT_IMPLEMENTED"
  )

  expect_error(
    radbc_connection_get_table_types(con),
    "ADBC_STATUS_NOT_IMPLEMENTED"
  )

  expect_error(
    radbc_connection_read_partition(con, raw()),
    "ADBC_STATUS_NOT_IMPLEMENTED"
  )

  expect_identical(
    radbc_connection_commit(con),
    con
  )

  expect_identical(
    radbc_connection_rollback(con),
    con
  )
})

test_that("can initialize and release a statement", {
  db <- radbc_database_init(radbc_driver_void())
  con <- radbc_connection_init(db)
  stmt <- radbc_statement_init(con, c("some_key" = "some_value"))
  expect_s3_class(stmt, "adbc_statement")
  radbc_statement_release(stmt)
  expect_error(radbc_statement_release(stmt), "ADBC_STATUS_INVALID_STATE")
})

test_that("statement methods work for the void driver", {
  db <- radbc_database_init(radbc_driver_void())
  con <- radbc_connection_init(db)
  stmt <- radbc_statement_init(con)

  skip("TODO")
})
