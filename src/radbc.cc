#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <adbc.h>
#include "adbc_driver_manager.h"

#include "radbc.h"

extern "C" SEXP RAdbcDatabaseNew(SEXP database_xptr, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcDatabaseSetOption(SEXP database_xptr, SEXP key_sexp, SEXP value_sexp,
                                       SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcDatabaseInit(SEXP database_xptr, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcDatabaseRelease(SEXP database_xptr, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcConnectionNew(SEXP connection_xptr, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcConnectionSetOption(SEXP connection_xptr, SEXP key_sexp,
                                         SEXP value_sexp, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcConnectionInit(SEXP connection_xptr, SEXP database_xptr,
                                    SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcConnectionRelease(SEXP connection_xptr, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcConnectionGetInfo(SEXP connection_xptr, uint32_t* info_codes,
                                       size_t info_codes_length, SEXP out_stream_xptr,
                                       SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcConnectionGetObjects(SEXP connection_xptr, SEXP depth_sexp,
                                          SEXP catalog_sexp, SEXP db_schema_sexp,
                                          SEXP table_name_sexp, SEXP table_type_sexp,
                                          SEXP column_name_sexp, SEXP out_stream_xptr,
                                          SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcConnectionGetTableSchema(SEXP connection_xptr, SEXP catalog,
                                              SEXP db_schema, SEXP table_name,
                                              SEXP schema_xptr, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcConnectionGetTableTypes(SEXP connection_xptr, SEXP out_stream_xptr,
                                             SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcConnectionReadPartition(SEXP connection_xptr,
                                             SEXP serialized_partition_sexp,
                                             SEXP out_stream_xptr, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcConnectionCommit(SEXP connection_xptr, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcConnectionRollback(SEXP connection_xptr, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcStatementNew(SEXP connection_xptr, SEXP statement_xptr,
                                  SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcStatementRelease(SEXP statement_xptr, SEXP error_xptr) {
  return R_NilValue;
}

extern "C" SEXP RAdbcStatementExecuteQuery(SEXP statement_xptr, SEXP out_xptr,
                                           SEXP error_xptr) {
  // Return rows affected
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
  // Return rows affected
  return R_NilValue;
}
