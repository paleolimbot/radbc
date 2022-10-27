
radbc_database_init <- function(driver) {
  database <- .Call(RAdbcDatabaseNew, driver)
  error <- radbc_allocate_error()
  status <- .Call(RAdbcDatabaseInit, database, error)
  stop_for_error(status, error)
  database
}

radbc_database_release <- function(database) {
  error <- radbc_allocate_error()
  status <- .Call(RAdbcDatabaseRelease, database, error)
  stop_for_error(status, error)
}
