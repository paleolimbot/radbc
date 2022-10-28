
radbc_database_init <- function(driver, options = NULL) {
  database <- .Call(RAdbcDatabaseNew, driver)
  radbc_database_set_options(database, options)

  error <- radbc_allocate_error()
  status <- .Call(RAdbcDatabaseInit, database, error)
  stop_for_error(status, error)
  database
}

radbc_database_set_options <- function(database, options) {
  options <- key_value_options(options)
  error <- radbc_allocate_error()
  for (i in seq_along(options)) {
    status <- .Call(
      RAdbcDatabaseSetOption,
      database,
      names(options)[i],
      options[i],
      error
    )
    stop_for_error(status, error)
  }
  invisible(database)
}

radbc_database_release <- function(database) {
  error <- radbc_allocate_error()
  status <- .Call(RAdbcDatabaseRelease, database, error)
  stop_for_error(status, error)
}

radbc_connection_init <- function(database, options = NULL) {
  connection <- .Call(RAdbcConnectionNew)
  error <- radbc_allocate_error()
  status <- .Call(RAdbcConnectionInit, connection, database, error)
  stop_for_error(status, error)

  radbc_connection_set_options(connection, options)

  connection
}

radbc_connection_set_options <- function(connection, options) {
  options <- key_value_options(options)
  error <- radbc_allocate_error()
  for (i in seq_along(options)) {
    status <- .Call(
      RAdbcConnectionSetOption,
      connection,
      names(options)[i],
      options[i],
      error
    )
    stop_for_error(status, error)
  }
  invisible(connection)
}

radbc_connection_release <- function(connection) {
  error <- radbc_allocate_error()
  status <- .Call(RAdbcConnectionRelease, connection, error)
  stop_for_error(status, error)
}

radbc_connection_get_info <- function(connection, info_codes) {
  error <- radbc_allocate_error()
  out_stream <- nanoarrow::nanoarrow_allocate_array_stream()
  status <- .Call(RAdbcConnectionGetInfo, connection, info_codes, out_stream, error)
  stop_for_error(status, error)

  out_stream
}

radbc_connection_get_objects <- function(connection, depth, catalog, db_schema,
                                         table_name, table_type, column_name) {
  error <- radbc_allocate_error()
  out_stream <- nanoarrow::nanoarrow_allocate_array_stream()
  status <- .Call(
    RAdbcConnectionGetObjects,
    connection,
    depth,
    catalog,
    db_schema,
    table_name,
    table_type,
    column_name,
    out_stream,
    error
  )
  stop_for_error(status, error)

  out_stream
}

radbc_connection_get_table_schema <- function(connection, catalog, db_schema, table_name) {
  error <- radbc_allocate_error()
  out_schema <- nanoarrow::nanoarrow_allocate_schema()
  status <- .Call(
    RAdbcConnectionGetTableSchema,
    connection,
    catalog,
    db_schema,
    table_name,
    out_schema,
    error
  )
  stop_for_error(status, error)

  out_stream
}

radbc_connection_get_table_types <- function(connection) {
  error <- radbc_allocate_error()
  out_stream <- nanoarrow::nanoarrow_allocate_array_stream()
  status <- .Call(RAdbcConnectionGetTableTypes, connection, out_stream, error)
  stop_for_error(status, error)

  out_stream
}

radbc_connection_read_partition <- function(connection, serialized_partition) {
  error <- radbc_allocate_error()
  out_stream <- nanoarrow::nanoarrow_allocate_array_stream()
  status <- .Call(
    RAdbcConnectionReadPartition,
    connection,
    serialized_partition,
    out_stream,
    error
  )
  stop_for_error(status, error)

  out_stream
}

radbc_connection_commit <- function(connection) {
  error <- radbc_allocate_error()
  .Call(RAdbcConnectionCommit, connection, error)
  invisible(connection)
}

radbc_connection_rollback <- function(connection) {
  error <- radbc_allocate_error()
  .Call(RAdbcConnectionRollback, connection, error)
  invisible(connection)
}

radbc_statement_init <- function(connection, options = NULL) {
  statement <- .Call(RAdbcStatementNew, connection)
  radbc_statement_set_options(statement, options)
  statement
}

radbc_statement_set_options <- function(statement, options) {
  options <- key_value_options(options)
  error <- radbc_allocate_error()
  for (i in seq_along(options)) {
    status <- .Call(
      RAdbcStatementSetOption,
      statement,
      names(options)[i],
      options[i],
      error
    )
    stop_for_error(status, error)
  }
  invisible(statement)
}

radbc_statement_release <- function(statement) {
  error <- radbc_allocate_error()
  status <- .Call(RAdbcStatementRelease, statement, error)
  stop_for_error(status, error)
}
