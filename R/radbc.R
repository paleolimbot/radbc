
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
