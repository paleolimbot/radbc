#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <adbc.h>
#include "adbc_driver_manager.h"

#include "radbc.h"

SEXP RAdbcDatabaseNew(SEXP database_xptr, SEXP error_xptr) {
  
  return R_NilValue; }

SEXP RAdbcDatabaseSetOption(SEXP database_xptr, SEXP key_sexp, SEXP value_sexp,
                            SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcDatabaseInit(SEXP database_xptr, SEXP error_xptr) { return R_NilValue; }

SEXP RAdbcDatabaseRelease(SEXP database_xptr, SEXP error_xptr) { return R_NilValue; }

SEXP RAdbcConnectionNew(SEXP connection_xptr, SEXP error_xptr) { return R_NilValue; }

SEXP RAdbcConnectionSetOption(SEXP connection_xptr, SEXP key_sexp, SEXP value_sexp,
                              SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcConnectionInit(SEXP connection_xptr, SEXP database_xptr, SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcConnectionRelease(SEXP connection_xptr, SEXP error_xptr) { return R_NilValue; }

SEXP RAdbcConnectionGetInfo(SEXP connection_xptr, uint32_t* info_codes,
                            size_t info_codes_length, SEXP out_stream_xptr,
                            SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcConnectionGetObjects(SEXP connection_xptr, SEXP depth_sexp, SEXP catalog_sexp,
                               SEXP db_schema_sexp, SEXP table_name_sexp,
                               SEXP table_type_sexp, SEXP column_name_sexp,
                               SEXP out_stream_xptr, SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcConnectionGetTableSchema(SEXP connection_xptr, SEXP catalog, SEXP db_schema,
                                   SEXP table_name, SEXP schema_xptr, SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcConnectionGetTableTypes(SEXP connection_xptr, SEXP out_stream_xptr,
                                  SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcConnectionReadPartition(SEXP connection_xptr, SEXP serialized_partition_sexp,
                                  SEXP out_stream_xptr, SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcConnectionCommit(SEXP connection_xptr, SEXP error_xptr) { return R_NilValue; }

SEXP RAdbcConnectionRollback(SEXP connection_xptr, SEXP error_xptr) { return R_NilValue; }

SEXP RAdbcStatementNew(SEXP connection_xptr, SEXP statement_xptr, SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcStatementRelease(SEXP statement_xptr, SEXP error_xptr) { return R_NilValue; }

SEXP RAdbcStatementExecuteQuery(SEXP statement_xptr, SEXP out_xptr, SEXP error_xptr) {
  // Return rows affected
  return R_NilValue;
}

SEXP RAdbcStatementPrepare(SEXP statement_xptr, SEXP error_xptr) { return R_NilValue; }

SEXP RAdbcStatementSetSqlQuery(SEXP statement_xptr, SEXP query_sexp, SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcStatementSetSubstraitPlan(SEXP statement_xptr, SEXP plan_sexp,
                                    SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcStatementBind(SEXP statement_xptr, SEXP values_xptr, SEXP schema_xptr,
                        SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcStatementBindStream(SEXP statement_xptr, SEXP stream_xptr, SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcStatementGetParameterSchema(SEXP statement_xptr, SEXP out_schema_xptr,
                                      SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcStatementSetOption(SEXP statement_xptr, SEXP key_sexp, SEXP value_sexp,
                             SEXP error_xptr) {
  return R_NilValue;
}

SEXP RAdbcStatementExecutePartitions(SEXP statement_xptr, SEXP out_schema_xptr,
                                     SEXP partitions_xptr, SEXP error_xptr) {
  // Return rows affected
  return R_NilValue;
}
