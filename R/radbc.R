
#' Create an ADBC Database
#'
#' @param driver An radbc_driver
#' @param ... Driver-specific options. For the default method, these are
#'   named values that are converted to strings.
#' @param options A named `character()` or `list()` whose values are converted
#'   to strings.
#'
#' @return An object of class radbc_database
#' @export
#'
#' @examples
#' radbc_database_init(radbc_driver_void())
#'
radbc_database_init <- function(driver, ...) {
  UseMethod("radbc_database_init")
}

#' @export
radbc_database_init.default <- function(driver, ...) {
  radbc_database_init_default(driver, list(...))
}

#' @rdname radbc_database_init
#' @export
radbc_database_init_default <- function(driver, options) {
  database <- .Call(RAdbcDatabaseNew, driver)
  radbc_database_set_options(database, options)

  error <- radbc_allocate_error()
  status <- .Call(RAdbcDatabaseInit, database, error)
  stop_for_error(status, error)
  database
}

#' @rdname radbc_database_init
#' @export
radbc_database_set_options <- function(database, options) {
  options <- key_value_options(options)
  error <- radbc_allocate_error()
  for (i in seq_along(options)) {
    key <- names(options)[i]
    value <- options[i]
    status <- .Call(
      RAdbcDatabaseSetOption,
      database,
      key,
      value,
      error
    )
    stop_for_error(status, error)
    xptr_add_option(database, key, value)
  }
  invisible(database)
}

#' @rdname radbc_database_init
#' @export
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

  out_schema
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

radbc_statement_set_sql_query <- function(statement, query) {
  error <- radbc_allocate_error()
  status <- .Call(RAdbcStatementSetSqlQuery, statement, query, error)
  stop_for_error(status, error)
  invisible(statement)
}

radbc_statement_set_substrait_plan <- function(statement, plan) {
  error <- radbc_allocate_error()
  status <- .Call(RAdbcStatementSetSubstraitPlan, statement, plan, error)
  stop_for_error(status, error)
  invisible(statement)
}

radbc_statement_prepare <- function(statement) {
  error <- radbc_allocate_error()
  status <- .Call(RAdbcStatementPrepare, statement, error)
  stop_for_error(status, error)
  invisible(statement)
}

radbc_statement_get_parameter_schema <- function(statement) {
  error <- radbc_allocate_error()
  schema <- nanoarrow::nanoarrow_allocate_schema()
  status <- .Call(RAdbcStatementGetParameterSchema, statement, schema, error)
  stop_for_error(status, error)
  schema
}

radbc_statement_bind <- function(statement, values, schema = NULL) {
  values <- nanoarrow::as_nanoarrow_array(values, schema = schema)
  schema <- nanoarrow::infer_nanoarrow_schema(values)
  error <- radbc_allocate_error()
  status <- .Call(RAdbcStatementBind, statement, values, schema, error)
  stop_for_error(status, error)
  invisible(statement)
}

radbc_statement_bind_stream <- function(statement, stream, schema = NULL) {
  stream <- nanoarrow::as_nanoarrow_array_stream(stream, schema = schema)
  error <- radbc_allocate_error()
  status <- .Call(RAdbcStatementBindStream, statement, stream, error)
  stop_for_error(status, error)
  invisible(statement)
}

radbc_statement_execute_query <- function(statement) {
  error <- radbc_allocate_error()
  out_stream <- nanoarrow::nanoarrow_allocate_array_stream()
  result <- .Call(RAdbcStatementExecuteQuery, statement, out_stream, error)
  stop_for_error(result$status, error)
  out_stream
}
