#' Internal function used in mod_games_table_server to format widget for mobile use
#' @noRd
format_mobile_widget <- function(data_list) {
  #browser()
  data <- data_list$data
  read_only_rows <- data_list$read_only_rows
  read_only_cols <- c("date_time", "teams", "scores")
  
  
  widget <- data %>% dplyr::mutate(date_time = format(date_time, format = "%d-%b %H:%M"),
                                   teams = make_team_abbrev(home_team, away_team)) %>%
    dplyr::select(date_time, teams, scores, home_pred, away_pred) %>%
    rhandsontable::rhandsontable(
      rowHeaders = NULL) %>%
    rhandsontable::hot_col(col = read_only_cols, readOnly = T) %>%
    rhandsontable::hot_row(row = read_only_rows, readOnly = T)
  
  widget
}
#' Internal function used in mod_games_table_server to format widget for PC use
#' @noRd
format_pc_widget <- function(data_list) {
  data <- data_list$data
  read_only_rows <- data_list$read_only_rows
  read_only_cols <- c("date_time", "home_team", "away_team", "scores", "submit_time")
  
  # Convert date_time variable to characters because rhandsontable do not support POSIXt and format
  data <- data %>% dplyr::mutate(date_time   = format(date_time, format = "%d-%b %H:%M"),
                                 submit_time = format(submit_time, format = "%d-%b %H:%M"))
  widget <- rhandsontable::rhandsontable(data,
                                         rowHeaders = NULL) %>%
    rhandsontable::hot_col(col = read_only_cols, readOnly = T) %>%
    rhandsontable::hot_row(row = read_only_rows, readOnly = T)
  widget
}

#' Internal function used in format_mobile_widget
#' @noRd
make_team_abbrev <- function(home_team, away_team){
  lookup <- c(
    "Brighton" = "BRH",
    "Burnley"  = "BUR",
    "Chelsea"  = "CHE",
    "Crystal Palace" = "CRY",
    "Everton" = "EVE",
    "Leicester" = "LEI",
    "Liverpool" = "LIV",
    "Manchester City" = "MCI",
    "Manchester United" = "MUN",
    "Newcastle" = "NEW",
    "Norwich" = "NOR",
    "Sheffield United" = "SHU",
    "Southampton" = "SOU",
    "Tottenham" = "TOT",
    "West Ham" = "WHU",
    "Wolverhampton" = "WLV")
  home_abbrev <- lookup[home_team] 
  away_abbrev <- lookup[away_team]
  paste0(home_abbrev,"-",away_abbrev)
}