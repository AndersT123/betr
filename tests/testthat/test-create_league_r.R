test_that("create_league_r creates directories with names as provided by league creation file", {
  # 1. Make temp dir to hold the directory structure
  # 2. Call create_league_r inside the temp directory
  # 3. Extract the directories created
  # 4. Check that they match with user names in league creation file (test-league.csv)
  tmp <- tempdir()
  create_league_r(league_name = file.path(tmp, "test-league"),
                  file_name   = system.file("extdata", "test-league.csv", package = "golEMTEST"))
  user_dirs <- list.dirs(file.path(tmp, "test-league"), full.names = F, recursive = F) %>% sort()
  user_names <- readr::read_csv(system.file("extdata", "test-league.csv", package = "golEMTEST"))$user_name %>% sort() 
  
  expect_equal(user_dirs, user_names)
})

test_that("meta.csv contains names as provided by league creation file", {
  tmp <- tempdir()
  create_league_r(league_name = file.path(tmp, "test-league2"),
                  file_name   = system.file("extdata", "test-league.csv", package = "golEMTEST"))
  user_meta <- readr::read_csv(file.path(tmp, "test-league2", "meta.csv"))$user_name %>% sort()
  user_csv  <- readr::read_csv(system.file("extdata", "test-league.csv", package = "golEMTEST"))$user_name %>% sort()
  expect_equal(user_meta, user_csv)
})
