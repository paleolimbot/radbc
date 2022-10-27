#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <cstring>

#include <adbc.h>
#include "radbc.h"

static void finalize_error_xptr(SEXP error_xptr) {
  auto error = reinterpret_cast<AdbcError*>(R_ExternalPtrAddr(error_xptr));
  if (error != nullptr && error->release != nullptr) {
    error->release(error);
  }

  radbc_xptr_default_finalize<AdbcError>(error_xptr);
}

SEXP RAdbcAllocateError(SEXP shelter_sexp) {
  SEXP error_xptr = PROTECT(radbc_allocate_xptr<AdbcError>(shelter_sexp));
  R_RegisterCFinalizer(error_xptr, &finalize_error_xptr);

  AdbcError* error = radbc_from_xptr<AdbcError>(error_xptr);
  error->message = nullptr;
  error->vendor_code = 0;
  memset(error->sqlstate, 0, sizeof(error->sqlstate));
  error->release = nullptr;

  UNPROTECT(1);
  return error_xptr;
}
