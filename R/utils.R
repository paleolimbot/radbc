
radbc_allocate_error <- function(shelter = NULL) {
  .Call(RAdbcAllocateError, shelter)
}
