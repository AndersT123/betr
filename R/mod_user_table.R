#' user_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_user_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    tableOutput(ns("table"))
  )
}
    
#' user_table Server Function
#'
#' @noRd 
mod_user_table_server <- function(input, output, session,  league_table){
  ns <- session$ns
  output$table <- renderTable({
    league_table
    })
}