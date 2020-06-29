#' games-table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session,user_name Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_games_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    checkboxInput(ns("mobile"), "Using Mobile?"),
    actionButton(ns("save_button") , "Save"),
    rhandsontable::rHandsontableOutput(ns("hot"))
  )
}
    
#' games-table Server Function
#'
#' @noRd 
mod_games_table_server <- function(input, output, session, to_database, user_name, league_games){
  ns <- session$ns
 
  #browser()    
  # Fetch specific user data from disk and join with league data
  data_list <- reactive(get_data(to_database = to_database, user_name = user_name(), league_games = league_games))
  
  # Create the output Handsontable to be displayed
  output$hot <- rhandsontable::renderRHandsontable({
    # update the table at every 15 minutes to ensure that cells will be locked if session is started close to game time
    # Not the best implementation as it will cause unsaved cells to be deleted when table is refreshed
    invalidateLater(1000 * 60 * 15)
    
    # Add the input$save_button to the reactive expression, such that the expression will recompute when it changes
    # This will make the submit_time variable update in the table that the user is viewing
    input$save_button
    
    # The widget, i.e. the table displayed to the user is different depending on if the user has ticked the mobile checkbox
    # and therefore we need to handle them differently. To this end, format_mobile_widget() and format_pc_widget() is defined.
    if(input$mobile) {
      widget <- format_mobile_widget(data_list())
    } else {
      widget <- format_pc_widget(data_list())
    }
    widget
  })
  
  observeEvent(input$save_button,
               {
                 #browser()
                 # The data to be saved is only the user predictions and the submit time of the predictions
                 
                 # input$hot can either be the mobile or pc version of the widgets. 
                 # They are converted to data.frames with hot_to_r
                 data <- rhandsontable::hot_to_r(input$hot)
                 # Ensure that the data written to CSV file contains:
                 #   date_time, home_team, away_team, home_pred, away_pred, submit_time
                 #
                 # There are two cases:
                 # 1. User is on PC and the data object created by hot_to_r(input$hot) contains:
                 #   date_time, home_team, away_team, scores, home_pred, away_pred, submit_time
                 #
                 # 2. User is on mobile (input$mobile is TRUE) and the data object created by hot_to_r(input$hot) contains:
                 #   date_time, teams_abbrev, scores,
                 
                 if(input$mobile) {
                   # Do something such that mobile data are identical to pc data
                   # data_list() is a reactive wraped around get_data() so it returns whatever get_data() returns.
                   # The reactive will not change during the user session, since the only reactive component is the user log-in
                   # Data list returns data: the input data joined with league-games and read_only_rows, which is a logical
                   # vector determining which rows that are read only in the widget.
                   not_in_mobile <- data_list()$data %>% dplyr::select(home_team, away_team, submit_time)
                   data <- dplyr::bind_cols(data, not_in_mobile) %>% 
                     dplyr::select(date_time, home_team, away_team, scores, home_pred, away_pred, submit_time)
                 }
                 
                 # input$date should be replaced with a updating variable or server upstart time
                 # submit_time should only be set if the pred_ variables are valid for being set during the session
                 # and they are non-NA
                 
                 # Convert the data$date_time and data$submit_time to date-time variables
                 # Add year to string such that data$date_time character can be converted to string
                 # warnings produced about formats failing to parse if NAs present
                 data <- data %>% dplyr::mutate(
                   date_time = lubridate::ydm_hm(paste0("2020-", date_time)),
                   submit_time = lubridate::ydm_hm(paste0("2020-", submit_time)))
                 
                 
                 condition <- (lubridate::now() - lubridate::minutes(30)) > data$date_time
                 
                 
                 
                 data <- data %>% dplyr::mutate(submit_time = dplyr::if_else(!condition &
                                                                 !is.na(home_pred) | !is.na(away_pred),
                                                               lubridate::now(),
                                                               submit_time)) %>%
                   dplyr::select(date_time, home_team, away_team, home_pred, away_pred, submit_time)
                 
                 
                 readr::write_csv(data, path = paste0(to_database,"/", user_name(), "/games.csv"))
               })
}