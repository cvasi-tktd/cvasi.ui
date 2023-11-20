#' forcings_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_forcings_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h2("Forcings")
  )
}
    
#' forcings_input Server Functions
#'
#' @noRd 
mod_forcings_input_server <- function(id, selected_model, forcings_time_series){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #f <- forcing_defaults[[input[["active_model"]]]]
    #forcings_time_series(f)
    
    # f <- forcing_defaults[[input[["active_model"]]]]
    # selected_model(selected_model() %>% 
    #                  set_forcings(f)
    #                )
    
    
    
  })
}
    
## To be copied in the UI
# mod_forcings_input_ui("forcings_input_1")
    
## To be copied in the server
# mod_forcings_input_server("forcings_input_1")
