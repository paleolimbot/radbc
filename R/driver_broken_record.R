
#' Always return the same data
#'
#' A variant of the broken_record driver that always returns the same data set.
#'
#' @return An object of class 'radbc_driver_broken_record'
#' @export
#'
#' @examples
#' db <- radbc_database_init(radbc_driver_broken_record(mtcars))
#' con <- radbc_connection_init(db)
#' stmt <- radbc_statement_init(con)
#' result <- radbc_statement_execute_query(stmt)
#' as.data.frame(result$get_next())
#'
radbc_driver_broken_record <- function(data) {
  # Not caching, because data may be different
  radbc_driver(
    .Call(RAdbcMonkeyDriverInitFunc),
    data = data,
    subclass = "radbc_driver_broken_record"
  )
}

#' @export
radbc_database_init.radbc_driver_broken_record <- function(driver, ...) {
  out <- radbc_database_init_default(driver, subclass = "radbc_database_broken_record")
  out$data <- driver$data
  out
}

#' @export
radbc_connection_init.radbc_database_broken_record <- function(database, ...) {
  out <- radbc_connection_init_default(database, subclass = "radbc_connection_broken_record")
  out$data <- database$data
  out
}

#' @export
radbc_statement_init.radbc_connection_broken_record <- function(connection, ...) {
  stream <- nanoarrow::as_nanoarrow_array_stream(connection$data)
  options <- list(
    result_stream_address = nanoarrow::nanoarrow_pointer_addr_chr(stream),
    ...
  )
  radbc_statement_init_default(connection, options, subclass = "radbc_statement_broken_record")
}
