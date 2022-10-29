
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

  expect_error(
    radbc_statement_set_sql_query(stmt, "some query"),
    "ADBC_STATUS_NOT_IMPLEMENTED"
  )

  expect_error(
    radbc_statement_set_substrait_plan(stmt, charToRaw("some plan")),
    "ADBC_STATUS_NOT_IMPLEMENTED"
  )

  expect_error(
    radbc_statement_prepare(stmt),
    "ADBC_STATUS_NOT_IMPLEMENTED"
  )

  expect_error(
    radbc_statement_get_parameter_schema(stmt),
    "ADBC_STATUS_NOT_IMPLEMENTED"
  )

  struct_array <- nanoarrow::as_nanoarrow_array(data.frame(x = 1:5))
  expect_error(
    radbc_statement_bind(stmt, struct_array),
    "ADBC_STATUS_NOT_IMPLEMENTED"
  )

  expect_error(
    radbc_statement_bind_stream(stmt, nanoarrow::nanoarrow_allocate_array_stream()),
    "ADBC_STATUS_NOT_IMPLEMENTED"
  )

  expect_error(
    radbc_statement_execute_query(stmt),
    "ADBC_STATUS_NOT_IMPLEMENTED"
  )
})

test_that("invalid parameter types generate errors", {
  db <- radbc_database_init(radbc_driver_void())
  con <- radbc_connection_init(db)
  stmt <- radbc_statement_init(con)

  expect_error(
    radbc_database_init(NULL),
    "Expected external pointer with class 'radbc_driver_init_func'"
  )

  expect_error(
    radbc_statement_set_sql_query(con, "some query"),
    "Expected external pointer with class 'adbc_statement'"
  )

  expect_error(
    radbc_connection_get_objects(
      con, NULL,
      "catalog", "db_schema",
      "table_name", "table_type", "column_name"
    ),
    "Expected integer(1) or double(1)",
    fixed = TRUE
  )

  expect_error(
    radbc_statement_set_sql_query(stmt, NULL),
    "Expected character(1)",
    fixed = TRUE
  )

  expect_error(
    radbc_statement_set_sql_query(stmt, NA_character_),
    "Can't convert NA_character_"
  )

  # (makes a NULL xptr)
  stmt2 <- unserialize(serialize(stmt, NULL))
  expect_error(
    radbc_statement_set_sql_query(stmt2, "some query"),
    "Can't convert external pointer to NULL to T*"
  )
})
