test_that("can access test-league.csv in external data", {
  expect_true(nchar(system.file("extdata","test-league.csv", package = "golEMTEST")) > 0)
})
