# Use same path as when create_league_r() is called

#' Load username listed in leagues meta.csv file
#' @param to_database path to database
load_users <- function(to_database) {
  credentials <- readr::read_csv(file.path(to_database, "meta.csv"),
                                 col_types = readr::cols(
                                   user_name = readr::col_character(),
                                   user_pwpass  = readr::col_character()
                                 ))
  user_names <- credentials %>% dplyr::pull("user_name")
  user_names
}

load_meta <- function(to_database) {
  credentials <- readr::read_csv(file.path(to_database, "meta.csv"),
                                 col_types = readr::cols(
                                   user_name = readr::col_character(),
                                   user_pwpass  = readr::col_character()
                                 ))
}
#' @param to_database path to database
load_user_games <- function(to_database) {
  print(environment())
  # Extract the directories that correspond to user data. This means exlcuding the data directory it self
  user_dirs <- list.dirs(to_database)[list.dirs(to_database) != path.expand(to_database)]
  # add filename to map over
  user_dirs <- paste0(user_dirs, "/games.csv")
  
  user_games <- purrr::map(user_dirs, readr::read_csv, col_types = readr::cols(
    date_time = readr::col_datetime(),
    home_team = readr::col_character(),
    away_team = readr::col_character(),
    home_pred = readr::col_integer(),
    away_pred = readr::col_integer(),
    submit_time = readr::col_datetime()),
    locale = readr::locale(tz = ""))
  
  purrr::set_names(user_games, load_users(golem::get_golem_options("to_database")))
}

#' Title
#' @param user_games,league_games 
join_w_league_games <- function(user_games, league_games) {
  scores_and_preds <- purrr::map(user_games, function(x) dplyr::inner_join(x, league_games,
                                                                           by = c("date_time", "home_team", "away_team")) %>%
                                   dplyr::select(home_score, home_pred, away_score, away_pred))
  scores_and_preds
}
# league_games <- scrape_games("27-01-2020", "01-03-2020")