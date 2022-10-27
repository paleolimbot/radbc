
radbc_allocate_error <- function(shelter = NULL) {
  .Call(RAdbcAllocateError, shelter)
}

#' @export
print.radbc_error <- function(x, ...) {
  str(x, ...)
}

#' @importFrom utils str
#' @export
str.radbc_error <- function(object, ...) {
  cat("<radbc_error> ")
  str(.Call(RAdbcErrorProxy, object), ...)
  invisible(object)
}

# This is the list()-like interface to nanoarrow_array_stream that allows $ and [[
# to make nice auto-complete when interacting in an IDE

#' @export
length.radbc_error <- function(x, ...) {
  3L
}

#' @export
names.radbc_error <- function(x, ...) {
  c("message", "vendor_code", "sqlstate")
}

#' @export
`[[.radbc_error` <- function(x, i, ...) {
  .Call(RAdbcErrorProxy, x)[[i]]
}

#' @export
`$.radbc_error` <- function(x, i, ...) {
  x[[i]]
}

