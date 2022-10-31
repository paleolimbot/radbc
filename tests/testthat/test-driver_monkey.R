
test_that("the monkey driver sees, and the monkey driver does", {
  db <- radbc_database_init(radbc_driver_monkey())
  expect_s3_class(db, "radbc_database_monkey")
  con <- radbc_connection_init(db)
  expect_s3_class(con, "radbc_connection_monkey")

  input <- data.frame(x = 1:10)
  stmt <- radbc_statement_init(con, input)
  expect_s3_class(stmt, "radbc_statement_monkey")
  result <- radbc_statement_execute_query(stmt)
  expect_identical(as.data.frame(result$get_next()), input)
})
