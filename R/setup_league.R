#' Set up a new league
#' 
#' It should be easy to manage setting up a new league. 
#' Use this function by passing in a configuration file to setup a 
#' database and populate table with all the relevant games for
#' the league.
#' 
#' @param config path to a configuration file with this information:
#' dbstuff:---
#' user:...
#' passwords:..
#' rapidapi: ...
#' league-setup:...
setup_league <- function() {
  config <- config::get()
  
  # 1) Build the games-table
  week_start <- config$league_setup$gameweek_start
  week_end <- config$league_setup$gameweek_end
  requests <- purrr::map(as.character(week_start:week_end), livescoresprl::get_match_request,
                         key = config$rapidapi$`x-rapidapi-key`)
  contents <- purrr::map(requests, httr::content)
  
  games <- tibble::tibble(
    game_time = purrr::flatten_chr(purrr::map(contents, livescoresprl::get_match_kickoff)),
    home_teams= purrr::flatten_chr(purrr::map(contents, livescoresprl::get_home_teams)),
    away_teams= purrr::flatten_chr(purrr::map(contents, livescoresprl::get_away_teams)),
    home_scores = NA_integer_,
    away_scores = NA_integer_
  )
  
  # 2) Build the predictions-table
  users <- users <- unlist(config$users)
  user_tables <- map(users, build_user_table)
  preds <- reduce(user_tables, dplyr::bind_rows)
}

#' Build a prediction table for a single user
#' 
#' Helper function for setup_league
#' 
#' @param user a character vector to map over
build_user_table <- function(user) {
  tibble::tibble(
    game_time = purrr::flatten_chr(purrr::map(contents, livescoresprl::get_match_kickoff)),
    home_teams= purrr::flatten_chr(purrr::map(contents, livescoresprl::get_home_teams)),
    away_teams= purrr::flatten_chr(purrr::map(contents, livescoresprl::get_away_teams)),
    home_preds = NA_integer_,
    away_preds = NA_integer_,
    users = user)
}

