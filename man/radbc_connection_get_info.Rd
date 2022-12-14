% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/radbc.R
\name{radbc_connection_get_info}
\alias{radbc_connection_get_info}
\alias{radbc_connection_get_objects}
\alias{radbc_connection_get_table_schema}
\alias{radbc_connection_get_table_types}
\alias{radbc_connection_read_partition}
\alias{radbc_connection_commit}
\alias{radbc_connection_rollback}
\title{Connection methods}
\usage{
radbc_connection_get_info(connection, info_codes)

radbc_connection_get_objects(
  connection,
  depth,
  catalog,
  db_schema,
  table_name,
  table_type,
  column_name
)

radbc_connection_get_table_schema(connection, catalog, db_schema, table_name)

radbc_connection_get_table_types(connection)

radbc_connection_read_partition(connection, serialized_partition)

radbc_connection_commit(connection)

radbc_connection_rollback(connection)
}
\arguments{
\item{connection}{An \link[=radbc_connection_init]{radbc_connection}}

\item{info_codes}{(Currently undocumented)}

\item{depth}{(Currently undocumented)}

\item{catalog}{(Currently undocumented)}

\item{db_schema}{(Currently undocumented)}

\item{table_name}{(Currently undocumented)}

\item{table_type}{(Currently undocumented)}

\item{column_name}{(Currently undocumented)}

\item{serialized_partition}{(Currently undocumented)}
}
\value{
\itemize{
\item \code{radbc_connection_get_info()}, \code{radbc_connection_get_objects},
\code{radbc_connection_get_table_types()}, and \code{radbc_connection_read_partition()}
return a \link[nanoarrow:as_nanoarrow_array_stream]{nanoarrow_array_stream}.
\item \code{radbc_connection_get_table_schema()} returns a
\link[nanoarrow:as_nanoarrow_schema]{nanoarrow_schena}
\item \code{radbc_connection_commit()} and \code{radbc_connection_rollback()} return
\code{connection}, invisibly.
}
}
\description{
Connection methods
}
\examples{
db <- radbc_database_init(radbc_driver_void())
con <- radbc_connection_init(db)
# (not implemented by the void driver)
try(radbc_connection_get_info(con, 0))

}
