#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <adbc.h>
#include "adbc_driver_manager.h"

#include "radbc.h"

static AdbcError global_error_;

static void radbc_global_error_init(void) {
  memset(&global_error_, 0, sizeof(AdbcError));
}

static void radbc_global_error_reset(void) {
  if (global_error_.release != nullptr) {
    global_error_.release(&global_error_);
  }

  radbc_global_error_init();
}

static const char* radbc_global_error_message() {
  if (global_error_.message == nullptr) {
    return "";
  } else {
    return global_error_.message;
  }
}

static void radbc_global_error_warn(int code, const char* context) {
  if (code != ADBC_STATUS_OK) {
    Rf_warning("<%s> %s", context, radbc_global_error_message());
  }
}

static void radbc_global_error_stop(int code, const char* context) {
  if (code != ADBC_STATUS_OK) {
    Rf_error("<%s> %s", context, radbc_global_error_message());
  }
}

static void finalize_database_xptr(SEXP database_xptr) {
  auto database = reinterpret_cast<AdbcDatabase*>(R_ExternalPtrAddr(database_xptr));
  if (database == nullptr) {
    return;
  }

  if (database->private_data != nullptr) {
    int status = AdbcDatabaseRelease(database, &global_error_);
    radbc_global_error_warn(status, "finalize_database_xptr()");
  }

  radbc_xptr_default_finalize<AdbcDatabase>(database_xptr);
}

extern "C" SEXP RAdbcDatabaseNew(SEXP driver_init_func_xptr) {
  auto driver_init_func =
      reinterpret_cast<AdbcDriverInitFunc>(R_ExternalPtrAddrFn(driver_init_func_xptr));
  if (!Rf_inherits(driver_init_func_xptr, "radbc_driver_init_func")) {
    Rf_error("Expected external pointer with class '%s'", "radbc_driver_init_func");
  }

  SEXP database_xptr = radbc_allocate_xptr<AdbcDatabase>();
  R_RegisterCFinalizer(database_xptr, &finalize_database_xptr);

  AdbcDatabase* database = radbc_from_xptr<AdbcDatabase>(database_xptr);
  radbc_global_error_reset();
  int status = AdbcDatabaseNew(database, &global_error_);
  radbc_global_error_stop(status, "RAdbcDatabaseNew()");

  radbc_global_error_reset();
  status =
      AdbcDriverManagerDatabaseSetInitFunc(database, driver_init_func, &global_error_);
  radbc_global_error_stop(status, "RAdbcDatabaseNew()");

  return database_xptr;
}

extern "C" SEXP RAdbcDatabaseSetOption(SEXP database_xptr, SEXP key_sexp, SEXP value_sexp,
                                       SEXP error_xptr) {
  auto database = radbc_from_xptr<AdbcDatabase>(database_xptr);
  const char* key = radbc_as_const_char(key_sexp);
  const char* value = radbc_as_const_char(value_sexp);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);
  return radbc_wrap_status(AdbcDatabaseSetOption(database, key, value, error));
}

extern "C" SEXP RAdbcDatabaseInit(SEXP database_xptr, SEXP error_xptr) {
  auto database = radbc_from_xptr<AdbcDatabase>(database_xptr);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);
  return radbc_wrap_status(AdbcDatabaseInit(database, error));
}

extern "C" SEXP RAdbcDatabaseRelease(SEXP database_xptr, SEXP error_xptr) {
  auto database = radbc_from_xptr<AdbcDatabase>(database_xptr);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);
  return radbc_wrap_status(AdbcDatabaseRelease(database, error));
}

static void finalize_connection_xptr(SEXP connection_xptr) {
  auto connection = reinterpret_cast<AdbcConnection*>(R_ExternalPtrAddr(connection_xptr));
  if (connection == nullptr) {
    return;
  }

  if (connection->private_data != nullptr) {
    int status = AdbcConnectionRelease(connection, &global_error_);
    radbc_global_error_warn(status, "finalize_connection_xptr()");
  }

  radbc_xptr_default_finalize<AdbcConnection>(connection_xptr);
}

extern "C" SEXP RAdbcConnectionNew() {
  SEXP connection_xptr = PROTECT(radbc_allocate_xptr<AdbcConnection>());
  R_RegisterCFinalizer(connection_xptr, &finalize_connection_xptr);

  AdbcConnection* connection = radbc_from_xptr<AdbcConnection>(connection_xptr);
  radbc_global_error_reset();
  int status = AdbcConnectionNew(connection, &global_error_);
  radbc_global_error_stop(status, "RAdbcConnectionNew()");

  UNPROTECT(1);
  return connection_xptr;
}

extern "C" SEXP RAdbcConnectionSetOption(SEXP connection_xptr, SEXP key_sexp,
                                         SEXP value_sexp, SEXP error_xptr) {
  auto connection = radbc_from_xptr<AdbcConnection>(connection_xptr);
  const char* key = radbc_as_const_char(key_sexp);
  const char* value = radbc_as_const_char(value_sexp);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);
  return radbc_wrap_status(AdbcConnectionSetOption(connection, key, value, error));
}

extern "C" SEXP RAdbcConnectionInit(SEXP connection_xptr, SEXP database_xptr,
                                    SEXP error_xptr) {
  auto connection = radbc_from_xptr<AdbcConnection>(connection_xptr);
  auto database = radbc_from_xptr<AdbcDatabase>(database_xptr);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);

  int result = AdbcConnectionInit(connection, database, error);
  if (result == ADBC_STATUS_OK) {
    // Keep the database pointer alive for as long as the connection pointer
    // is alive
    R_SetExternalPtrProtected(connection_xptr, database_xptr);
  }

  return radbc_wrap_status(result);
}

extern "C" SEXP RAdbcConnectionRelease(SEXP connection_xptr, SEXP error_xptr) {
  auto connection = radbc_from_xptr<AdbcConnection>(connection_xptr);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);
  int status = AdbcConnectionRelease(connection, error);
  R_SetExternalPtrProtected(connection_xptr, R_NilValue);
  return radbc_wrap_status(status);
}

extern "C" SEXP RAdbcConnectionGetInfo(SEXP connection_xptr, SEXP info_codes_sexp,
                                       SEXP out_stream_xptr, SEXP error_xptr) {
  auto connection = radbc_from_xptr<AdbcConnection>(connection_xptr);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);
  auto out_stream = radbc_from_xptr<ArrowArrayStream>(out_stream_xptr);
  auto info_codes = reinterpret_cast<uint32_t*>(INTEGER(info_codes_sexp));
  size_t info_codes_length = Rf_xlength(info_codes_sexp);
  int status =
      AdbcConnectionGetInfo(connection, info_codes, info_codes_length, out_stream, error);
  return radbc_wrap_status(status);
}

extern "C" SEXP RAdbcConnectionGetObjects(SEXP connection_xptr, SEXP depth_sexp,
                                          SEXP catalog_sexp, SEXP db_schema_sexp,
                                          SEXP table_name_sexp, SEXP table_type_sexp,
                                          SEXP column_name_sexp, SEXP out_stream_xptr,
                                          SEXP error_xptr) {
  auto connection = radbc_from_xptr<AdbcConnection>(connection_xptr);
  int depth = radbc_as_int(depth_sexp);
  const char* catalog = radbc_as_const_char(catalog_sexp);
  const char* db_schema = radbc_as_const_char(db_schema_sexp);
  const char* table_name = radbc_as_const_char(table_name_sexp);

  int table_type_length = Rf_length(table_type_sexp);
  SEXP table_type_shelter =
      PROTECT(Rf_allocVector(RAWSXP, (table_type_length + 1) * sizeof(const char*)));
  auto table_type = reinterpret_cast<const char**>(RAW(table_type_shelter));
  for (int i = 0; i < table_type_length; i++) {
    table_type[i] = Rf_translateCharUTF8(STRING_ELT(table_type_sexp, i));
  }
  table_type[table_type_length] = nullptr;

  const char* column_name = radbc_as_const_char(column_name_sexp);
  auto out_stream = radbc_from_xptr<ArrowArrayStream>(out_stream_xptr);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);

  int status = AdbcConnectionGetObjects(connection, depth, catalog, db_schema, table_name,
                                        table_type, column_name, out_stream, error);
  UNPROTECT(1);
  return radbc_wrap_status(status);
}

extern "C" SEXP RAdbcConnectionGetTableSchema(SEXP connection_xptr, SEXP catalog_sexp,
                                              SEXP db_schema_sexp, SEXP table_name_sexp,
                                              SEXP schema_xptr, SEXP error_xptr) {
  auto connection = radbc_from_xptr<AdbcConnection>(connection_xptr);
  const char* catalog = radbc_as_const_char(catalog_sexp);
  const char* db_schema = radbc_as_const_char(db_schema_sexp);
  const char* table_name = radbc_as_const_char(table_name_sexp);
  auto schema = radbc_from_xptr<ArrowSchema>(schema_xptr);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);

  int status = AdbcConnectionGetTableSchema(connection, catalog, db_schema, table_name,
                                            schema, error);
  return radbc_wrap_status(status);
}

extern "C" SEXP RAdbcConnectionGetTableTypes(SEXP connection_xptr, SEXP out_stream_xptr,
                                             SEXP error_xptr) {
  auto connection = radbc_from_xptr<AdbcConnection>(connection_xptr);
  auto out_stream = radbc_from_xptr<ArrowArrayStream>(out_stream_xptr);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);

  int status = AdbcConnectionGetTableTypes(connection, out_stream, error);
  return radbc_wrap_status(status);
}

extern "C" SEXP RAdbcConnectionReadPartition(SEXP connection_xptr,
                                             SEXP serialized_partition_sexp,
                                             SEXP out_stream_xptr, SEXP error_xptr) {
  auto connection = radbc_from_xptr<AdbcConnection>(connection_xptr);
  auto serialized_partition = reinterpret_cast<const uint8_t*>(serialized_partition_sexp);
  uint32_t serialized_length = Rf_xlength(serialized_partition_sexp);
  auto out_stream = radbc_from_xptr<ArrowArrayStream>(out_stream_xptr);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);

  int status = AdbcConnectionReadPartition(connection, serialized_partition,
                                           serialized_length, out_stream, error);
  return radbc_wrap_status(status);
}

extern "C" SEXP RAdbcConnectionCommit(SEXP connection_xptr, SEXP error_xptr) {
  auto connection = radbc_from_xptr<AdbcConnection>(connection_xptr);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);
  int status = AdbcConnectionCommit(connection, error);
  return radbc_wrap_status(status);
}

extern "C" SEXP RAdbcConnectionRollback(SEXP connection_xptr, SEXP error_xptr) {
  auto connection = radbc_from_xptr<AdbcConnection>(connection_xptr);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);
  int status = AdbcConnectionCommit(connection, error);
  return radbc_wrap_status(status);
}

static void finalize_statement_xptr(SEXP statement_xptr) {
  auto statement = reinterpret_cast<AdbcStatement*>(R_ExternalPtrAddr(statement_xptr));
  if (statement == nullptr) {
    return;
  }

  if (statement->private_data != nullptr) {
    int status = AdbcStatementRelease(statement, &global_error_);
    radbc_global_error_warn(status, "finalize_statement_xptr()");
  }

  radbc_xptr_default_finalize<AdbcStatement>(statement_xptr);
}

extern "C" SEXP RAdbcStatementNew(SEXP connection_xptr) {
  auto connection = radbc_from_xptr<AdbcConnection>(connection_xptr);
  SEXP statement_xptr = PROTECT(radbc_allocate_xptr<AdbcStatement>(connection_xptr));
  R_RegisterCFinalizer(statement_xptr, &finalize_statement_xptr);

  AdbcStatement* statement = radbc_from_xptr<AdbcStatement>(statement_xptr);
  radbc_global_error_reset();
  int status = AdbcStatementNew(connection, statement, &global_error_);
  radbc_global_error_stop(status, "RAdbcStatementNew()");

  R_SetExternalPtrProtected(statement_xptr, connection_xptr);

  UNPROTECT(1);
  return statement_xptr;
}

extern "C" SEXP RAdbcStatementRelease(SEXP statement_xptr, SEXP error_xptr) {
  auto statement = radbc_from_xptr<AdbcStatement>(statement_xptr);
  auto error = radbc_from_xptr<AdbcError>(error_xptr);
  int status = AdbcStatementRelease(statement, error);
  R_SetExternalPtrProtected(statement_xptr, R_NilValue);
  return radbc_wrap_status(status);
}

extern "C" SEXP RAdbcStatementExecuteQuery(SEXP statement_xptr, SEXP out_xptr,
                                           SEXP error_xptr) {
  // rows affected?
  return R_NilValue;
}

extern "C" SEXP RAdbcStatementPrepare(SEXP statement_xptr, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcStatementSetSqlQuery(SEXP statement_xptr, SEXP query_sexp,
                                          SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcStatementSetSubstraitPlan(SEXP statement_xptr, SEXP plan_sexp,
                                               SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcStatementBind(SEXP statement_xptr, SEXP values_xptr,
                                   SEXP schema_xptr, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcStatementBindStream(SEXP statement_xptr, SEXP stream_xptr,
                                         SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcStatementGetParameterSchema(SEXP statement_xptr,
                                                 SEXP out_schema_xptr, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcStatementSetOption(SEXP statement_xptr, SEXP key_sexp,
                                        SEXP value_sexp, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcStatementExecutePartitions(SEXP statement_xptr, SEXP out_schema_xptr,
                                                SEXP partitions_xptr, SEXP error_xptr) {
  // rows affected?
  return R_NilValue;
}
