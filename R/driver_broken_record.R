
#' Always return the same data
#'
#' A variant of the [radbc_driver_monkey()] that always returns the same data set.
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
  force(data)
  stopifnot(is.data.frame(data))
  stream_fun <- function() {
    stream <- nanoarrow::as_nanoarrow_array_stream(data)
    nanoarrow::nanoarrow_pointer_addr_chr(stream)
  }

  broken_record_env$stream_fun <- stream_fun

  if (is.null(internal_driver_env$broken_record)) {
    internal_driver_env$broken_record <- radbc_driver(
      .Call(RAdbcMonkeyDriverInitFunc),
      subclass = "radbc_driver_broken_record"
    )
  }

  internal_driver_env$broken_record
}

broken_record_env <- new.env(parent = emptyenv())

broken_record_get_stream <- function() {
  broken_record_env$stream_fun()
}

#' @export
radbc_database_init.radbc_driver_broken_record <- function(driver, ...) {
  radbc_database_init_default(driver, subclass = "radbc_database_broken_record")
}

#' @export
radbc_connection_init.radbc_database_broken_record <- function(database, ...) {
  radbc_connection_init_default(database, subclass = "radbc_connection_broken_record")
}

#' @export
radbc_statement_init.radbc_connection_broken_record <- function(connection, ...) {
  options <- list(
    ...
  )
  radbc_statement_init_default(connection, options, subclass = "radbc_statement_broken_record")
}
