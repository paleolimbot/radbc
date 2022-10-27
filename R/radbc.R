
radbc_database_new <- function(driver_init_func) {
  .Call(RAdbcDatabaseNew, driver_init_func)
}


