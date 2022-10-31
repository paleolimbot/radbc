
#' Databases
#'
#' @param driver An [radbc_driver()].
#' @param database An [radbc_database][radbc_database_init].
#' @param ... Driver-specific options. For the default method, these are
#'   named values that are converted to strings.
#' @param options A named `character()` or `list()` whose values are converted
#'   to strings.
#' @param subclass An extended class for an object so that drivers can specify
#'   finer-grained control over behaviour at the R level.
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
radbc_database_init_default <- function(driver, options, subclass = character()) {
  database <- .Call(RAdbcDatabaseNew, driver$driver_init_func)
  database$driver <- driver
  radbc_database_set_options(database, options)

  error <- radbc_allocate_error()
  status <- .Call(RAdbcDatabaseInit, database, error)
  stop_for_error(status, error)
  class(database) <- c(subclass, class(database))
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
  invisible(database)
}

#' Connections
#'
#' @inheritParams radbc_database_init
#' @param connection An [radbc_connection][radbc_connection_init]
#'
#' @return An object of class 'radbc_connection'
#' @export
#'
#' @examples
#' db <- radbc_database_init(radbc_driver_void())
#' radbc_connection_init(db)
#'
radbc_connection_init <- function(database, ...) {
  UseMethod("radbc_connection_init")
}

#' @export
radbc_connection_init.default <- function(database, ...) {
  radbc_connection_init_default(database, list(...))
}

#' @rdname radbc_connection_init
#' @export
radbc_connection_init_default <- function(database, options = NULL, subclass = character()) {
  connection <- .Call(RAdbcConnectionNew)
  connection$database <- database
  error <- radbc_allocate_error()
  status <- .Call(RAdbcConnectionInit, connection, database, error)
  stop_for_error(status, error)

  radbc_connection_set_options(connection, options)
  class(connection) <- c(subclass, class(connection))
  connection
}

#' @rdname radbc_connection_init
#' @export
radbc_connection_set_options <- function(connection, options) {
  options <- key_value_options(options)
  error <- radbc_allocate_error()
  for (i in seq_along(options)) {
    key <- names(options)[i]
    value <- options[i]
    status <- .Call(
      RAdbcConnectionSetOption,
      connection,
      key,
      value,
      error
    )
    stop_for_error(status, error)
    xptr_add_option(connection, key, value)
  }
  invisible(connection)
}

#' @rdname radbc_connection_init
#' @export
radbc_connection_release <- function(connection) {
  error <- radbc_allocate_error()
  status <- .Call(RAdbcConnectionRelease, connection, error)
  stop_for_error(status, error)
  invisible(connection)
}


#' Connection methods
#'
#' @inheritParams radbc_connection_init
#' @param info_codes (Currently undocumented)
#' @param depth (Currently undocumented)
#' @param catalog (Currently undocumented)
#' @param db_schema (Currently undocumented)
#' @param table_name (Currently undocumented)
#' @param table_type (Currently undocumented)
#' @param column_name (Currently undocumented)
#' @param serialized_partition (Currently undocumented)
#'
#' @return
#'   - `radbc_connection_get_info()`, `radbc_connection_get_objects`,
#'     `radbc_connection_get_table_types()`, and `radbc_connection_read_partition()`
#'     return a [nanoarrow_array_stream][nanoarrow::as_nanoarrow_array_stream()].
#'   - `radbc_connection_get_table_schema()` returns a
#'     [nanoarrow_schena][nanoarrow::as_nanoarrow_schema()]
#'   - `radbc_connection_commit()` and `radbc_connection_rollback()` return
#'     `connection`, invisibly.
#' @export
#'
#' @examples
#' db <- radbc_database_init(radbc_driver_void())
#' con <- radbc_connection_init(db)
#' # (not implemented by the void driver)
#' try(radbc_connection_get_info(con, 0))
#'
radbc_connection_get_info <- function(connection, info_codes) {
  error <- radbc_allocate_error()
  out_stream <- nanoarrow::nanoarrow_allocate_array_stream()
  status <- .Call(RAdbcConnectionGetInfo, connection, info_codes, out_stream, error)
  stop_for_error(status, error)

  out_stream
}

#' @rdname radbc_connection_get_info
#' @export
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

#' @rdname radbc_connection_get_info
#' @export
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

#' @rdname radbc_connection_get_info
#' @export
radbc_connection_get_table_types <- function(connection) {
  error <- radbc_allocate_error()
  out_stream <- nanoarrow::nanoarrow_allocate_array_stream()
  status <- .Call(RAdbcConnectionGetTableTypes, connection, out_stream, error)
  stop_for_error(status, error)

  out_stream
}

#' @rdname radbc_connection_get_info
#' @export
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

#' @rdname radbc_connection_get_info
#' @export
radbc_connection_commit <- function(connection) {
  error <- radbc_allocate_error()
  .Call(RAdbcConnectionCommit, connection, error)
  invisible(connection)
}

#' @rdname radbc_connection_get_info
#' @export
radbc_connection_rollback <- function(connection) {
  error <- radbc_allocate_error()
  .Call(RAdbcConnectionRollback, connection, error)
  invisible(connection)
}

#' Statements
#'
#' @inheritParams radbc_connection_init
#' @param statement An [radbc_statement][radbc_statement_init]
#'
#' @return An object of class 'radbc_statement'
#' @export
#'
#' @examples
#' db <- radbc_database_init(radbc_driver_void())
#' con <- radbc_connection_init(db)
#' radbc_statement_init(con)
#'
radbc_statement_init <- function(connection, ...) {
  UseMethod("radbc_statement_init")
}

#' @export
radbc_statement_init.default <- function(connection, ...) {
  radbc_statement_init_default(connection, list(...))
}

#' @rdname radbc_statement_init
#' @export
radbc_statement_init_default <- function(connection, options = NULL, subclass = character()) {
  statement <- .Call(RAdbcStatementNew, connection)
  statement$connection <- connection
  radbc_statement_set_options(statement, options)
  class(statement) <- c(subclass, class(statement))
  statement
}

#' @rdname radbc_statement_init
#' @export
radbc_statement_set_options <- function(statement, options) {
  options <- key_value_options(options)
  error <- radbc_allocate_error()
  for (i in seq_along(options)) {
    key <- names(options)[i]
    value <- options[i]
    status <- .Call(
      RAdbcStatementSetOption,
      statement,
      key,
      value,
      error
    )
    stop_for_error(status, error)
    xptr_add_option(statement, key, value)
  }
  invisible(statement)
}

#' @rdname radbc_statement_init
#' @export
radbc_statement_release <- function(statement) {
  error <- radbc_allocate_error()
  status <- .Call(RAdbcStatementRelease, statement, error)
  stop_for_error(status, error)
  invisible(statement)
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
