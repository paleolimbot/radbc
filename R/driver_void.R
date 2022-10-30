
#' Create ADBC drivers
#'
#' Creates the R object representation of an ADBC driver, which consists of a
#' name and an initializer function with an optional subclass to control
#' finer-grained behaviour at the R level.
#'
#' @param driver_init_func An external pointer to a DL_FUNC with the type
#'   `AdbcDriverInitFunc` specified in the adbc.h header. The external pointer
#'   must have the class "radbc_driver_init_func".
#' @param ... Further key/value parameters to store with the (R-level) driver
#'   object.
#' @param subclass An optional subclass for finer-grained control of
#'   behaviour at the R level.
#'
#' @return An object of class 'radbc_driver'
#' @export
#'
#' @examples
#' radbc_driver_void()
#'
radbc_driver_void <- function() {
  if (is.null(internal_driver_env$void)) {
    internal_driver_env$void <- radbc_driver(
      .Call(RAdbcVoidDriverInitFunc),
      subclass = "radbc_driver_void"
    )
  }

  internal_driver_env$void
}

#' @rdname radbc_driver_void
#' @export
radbc_driver <- function(driver_init_func, ...,  subclass = character()) {
  stopifnot(inherits(driver_init_func, "radbc_driver_init_func"))

  structure(
    as.environment(list(driver_init_func = driver_init_func, ...)),
    class = c(subclass, "radbc_driver")
  )
}

internal_driver_env <- new.env(parent = emptyenv())

#' @export
print.radbc_driver <- function(x, ...) {
  str(x, ...)
}

#' @importFrom utils str
#' @export
str.adbc_driver <- function(object, ...) {
  cat(sprintf("<%s> ", class(object)[1]))
  str(object, ...)
  invisible(object)
}
