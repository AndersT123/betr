#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  to_database <- golem::get_golem_options("to_database")
  meta <- load_meta(to_database = to_database)
  user_names <- meta %>% dplyr::pull("user_name")
  #user_names <- load_users(to_database = golem::get_golem_options("to_database"))
  user_games <- load_user_games(to_database = to_database)
  print(user_games)
  scores_and_preds <- join_w_league_games(user_games, league_games = league_games)
  user_points <- purrr::map(scores_and_preds, function(x) purrr::pmap_dbl(x, compute_game_score))
  user_points_sum <- purrr::map(user_points, sum, na.rm = T) %>% unlist() %>% as.integer()
  league_table <- tibble::tibble("User Name" = user_names, "Points" = user_points_sum) %>% dplyr::arrange(dplyr::desc(Points))
  
  
  login_mod <- callModule(mod_loginpage_server, "loginpage_ui_1", credentials = meta)
  callModule(mod_games_table_server, "games_table_ui_1", to_database = to_database, reactive({login_mod$user()}), league_games = league_games)
  callModule(mod_plyd_games_server, "plyd_games_ui_1", user_games = user_games, league_games = league_games)
  callModule(mod_user_table_server, "user_table_ui_1", league_table = league_table)
  callModule(mod_user_standings_server, "user_standings_ui_1", user_games = user_games, user_names = user_names, league_games = league_games)
  
  output$logoutbtn <- renderUI({
    req(login_mod$login())
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
            font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (login_mod$login() == TRUE){ 
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard"))
      )
    }
  })
  
  output$body <- renderUI({
    if (login_mod$login() == TRUE) {
      shinydashboard::tabItem(tabName ="dashboard", class = "active",
              fluidRow(
                column(6, 
                       shinydashboard::box(width = NULL, mod_games_table_ui("games_table_ui_1"))
                ),
                column(6, 
                       shinydashboard::box(width = NULL, title = "Current Standings",
                           mod_user_table_ui("user_table_ui_1")),
                       shinydashboard::box(width = NULL, 
                           mod_user_standings_ui("user_standings_ui_1")),
                       shinydashboard::box(width = NULL, mod_plyd_games_ui("plyd_games_ui_1", user_names = user_names))
                )
              )
              
      )
    }
    else { 
      mod_loginpage_ui("loginpage_ui_1")
    }
  })
}
