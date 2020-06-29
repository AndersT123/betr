#' plyd_games UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plyd_games_ui <- function(id, user_names){
  ns <- NS(id)
  tagList(
    selectInput(ns("plyd_user"), label = "Select user", choices = user_names),
    tableOutput(ns("plyd_table"))
  )
}

#' plyd_games Server Function
#'
#' @noRd 
mod_plyd_games_server <- function(input, output, session, user_games, league_games){
  ns <- session$ns
  print(user_games)
  output$plyd_table <- renderTable({
    # df <- readr::read_csv(file.path(to_database, input$plyd_user, "games.csv"),
    #                col_types = cols(
    #                  date_time = col_datetime(),
    #                  home_team = col_character(),
    #                  away_team = col_character(),
    #                  home_pred = col_integer(),
    #                  away_pred = col_integer()))
    
    df <- user_games[[input$plyd_user]]
    print(df)
    # join with league games to get the actual game scores for the played games
    df  <- dplyr::inner_join(df, league_games) %>%
      # remove games that have not been played yet, i.e. include only games with valid scores
      dplyr::filter(!is.na(home_score) & !is.na(away_score))
    
    
    df$points <- purrr::pmap_dbl(df, compute_game_score) %>% as.integer()
    
    df <- df %>% dplyr::select(home_team, away_team, home_pred, away_pred, points)
  })
}