
#pragma once

#include <R.h>
#include <Rinternals.h>

template <typename T>
static inline const char* radbc_xptr_class();

template <>
inline const char* radbc_xptr_class<AdbcError>() {
    return "radbc_error";
}

template <>
inline const char* radbc_xptr_class<AdbcDriver>() {
    return "radbc_driver";
}

template <>
inline const char* radbc_xptr_class<AdbcDatabase>() {
    return "radbc_database";
}

template <>
inline const char* radbc_xptr_class<AdbcConnection>() {
    return "radbc_connection";
}

template <>
inline const char* radbc_xptr_class<AdbcStatement>() {
    return "adbc_statement";
}

template <typename T>
static inline T* radbc_from_xptr(SEXP xptr) {
  T* ptr = reinterpret_cast<T*>(R_ExternalPtrAddr(xptr));
  if (ptr == nullptr) {
    Rf_error("Can't convert external pointer to NULL to T*");
  }
  return ptr;
}

template <typename T>
static inline SEXP radbc_allocate_xptr(SEXP shelter_sexp = R_NilValue) {
  void* ptr = malloc(sizeof(T));
  if (ptr == nullptr) {
    Rf_error("Failed to allocate T");
  }

  memset(ptr, 0, sizeof(T));
  SEXP xptr = PROTECT(R_MakeExternalPtr(ptr, R_NilValue, shelter_sexp));
  Rf_setAttrib(xptr, R_ClassSymbol, Rf_mkString(radbc_xptr_class<T>()));
  UNPROTECT(1);
  return xptr;
}

template <typename T>
static inline void radbc_xptr_default_finalize(SEXP xptr) {
  T* ptr = reinterpret_cast<T*>(R_ExternalPtrAddr(xptr));
  if (ptr != nullptr) {
    free(ptr);
  }
}

static inline const char* radbc_as_const_char(SEXP sexp) {
  if (TYPEOF(sexp) != STRSXP || Rf_length(sexp) != 1) {
    Rf_error("Expected character(1) for conversion to const char*");
  }

  SEXP item = STRING_ELT(sexp, 0);
  if (item == NA_STRING) {
    Rf_error("Can't convert NA_character_ to const char*");
  }

  return Rf_translateCharUTF8(item);
}

static inline int radbc_as_int(SEXP sexp) {
  if (Rf_length(sexp) == 1) {
    switch (TYPEOF(sexp)) {
      case REALSXP:
        return REAL(sexp)[0];
      case INTSXP:
        return INTEGER(sexp)[0];
    }
  }

  Rf_error("Expected integer(1) or double(1) for conversion to int");
}

static inline SEXP radbc_wrap_status(AdbcStatusCode code) {
  return Rf_ScalarInteger(code);
}
