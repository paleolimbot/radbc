
#' Monkey see, monkey do!
#'
#' A driver whose query results are set in advance.
#'
#' @return An object of class 'radbc_driver_monkey'
#' @export
#'
#' @examples
#' db <- radbc_database_init(radbc_driver_monkey())
#' con <- radbc_connection_init(db)
#' stmt <- radbc_statement_init(con, mtcars)
#' result <- radbc_statement_execute_query(stmt)
#' as.data.frame(result$get_next())
#'
radbc_driver_monkey <- function() {
  if (is.null(internal_driver_env$monkey)) {
    internal_driver_env$monkey <- radbc_driver(
      .Call(RAdbcMonkeyDriverInitFunc),
      subclass = "radbc_driver_monkey"
    )
  }

  internal_driver_env$monkey
}

#' @export
radbc_database_init.radbc_driver_monkey <- function(driver, ...) {
  radbc_database_init_default(driver, subclass = "radbc_database_monkey")
}

#' @export
radbc_connection_init.radbc_database_monkey <- function(database, ...) {
  radbc_connection_init_default(database, subclass = "radbc_connection_monkey")
}

#' @export
radbc_statement_init.radbc_connection_monkey <- function(connection, stream, ...) {
  stream <- nanoarrow::as_nanoarrow_array_stream(stream)
  options <- list(
    result_stream_address = nanoarrow::nanoarrow_pointer_addr_chr(stream),
    ...
  )
  radbc_statement_init_default(connection, options, subclass = "radbc_statement_monkey")
}
