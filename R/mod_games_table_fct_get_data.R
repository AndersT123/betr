#' Read user specific data from disk and join with the league level data. Compute also which rows should be read only and
#' pass them along with the actual data in a list.
#' Internal function.
#' @noRd
get_data <- function(to_database, user_name, league_games) {
  #browser()
  #server_start <- server_start()
  
  # Load the latest saved user data
  data <- readr::read_csv(file.path(to_database, user_name, "games.csv"),
                          col_types = readr::cols(
                            date_time = readr::col_datetime(),
                            home_team = readr::col_character(),
                            away_team = readr::col_character(),
                            home_pred = readr::col_integer(),
                            away_pred = readr::col_integer(),
                            submit_time = readr::col_datetime()),
                          # This will read in the date-times as the system local. The time saved on disk is confusingly saved as UTF
                          # time and will be behind our time zone, but this is remidied by locale = locale(tz = "")
                          locale = readr::locale(tz = ""))
  # The latest saved league games data are loaded in global.R as league_table
  # Join user data with league games and format such that all relevant is presented in rhandsontable
  
  data <- dplyr::left_join(league_games, data, by = c("date_time", 'home_team', 'away_team'))
  
  data <- data %>% dplyr::mutate(scores = dplyr::if_else(!is.na(home_score) & !is.na(away_score),
                                                         paste0(home_score, "-", away_score),
                                                         NA_character_)) %>%
    dplyr::select(date_time, home_team, away_team, scores, home_pred, away_pred, submit_time)
  
  # Only games that are 30 mins away from starting should have editable prediction cells
  # Find a good solution for how to update this during the user is using the app
  # Conversion from character back to datetime to do the comparison with server_start
  #condition <- as_datetime(server_start) - minutes(30) > as_datetime(data$date_time)
  condition <- lubridate::now() - lubridate::minutes(30) > data$date_time
  read_only_rows <- which(condition)
  
  list(data = data, read_only_rows = read_only_rows)
}
