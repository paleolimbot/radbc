
key_value_options <- function(options) {
  if (!is.character(options)) {
    options <- vapply(options, as.character, character(1))
  }

  keys <- names(options)
  if (length(options) == 0) {
    names(options) <- character()
  } else if (is.null(keys) || all(keys == "")) {
    stop("key/value options must be named")
  }

  options
}

xptr_add_option <- function(xptr, key, value) {
  if (is.null(xptr$options)) {
    xptr$options <- new.env(parent = emptyenv())
  }

  xptr$options[[key]] <- unname(value)
}

new_env <- function() {
  new.env(parent = emptyenv())
}

xptr_env <- function(xptr) {
  .Call(RAdbcXptrEnv, xptr)
}

#' @export
length.radbc_xptr <- function(x) {
  length(xptr_env(x))
}

#' @export
names.radbc_xptr <- function(x) {
  names(xptr_env(x))
}


#' @export
`[[.radbc_xptr` <- function(x, i) {
  xptr_env(x)[[i]]
}

#' @export
`[[<-.radbc_xptr` <- function(x, i, value) {
  env <- xptr_env(x)
  env[[i]] <- value
  x
}

#' @export
`$.radbc_xptr` <- function(x, name) {
  xptr_env(x)[[name]]
}

#' @export
`$<-.radbc_xptr` <- function(x, name, value) {
  env <- xptr_env(x)
  env[[name]] <- value
  x
}

