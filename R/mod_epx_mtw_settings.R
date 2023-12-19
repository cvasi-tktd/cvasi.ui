#' epx_mtw_settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_epx_mtw_settings_ui <- function(id){
  ns <- NS(id)
  tagList(
    #actionButton(ns("debug"), "debug"),
    tags$div(
      numericInput(ns("level"), "Effect level", value = 10),
      numericInput(ns("factor_cutoff"), "Cutoff value", value = 1000),
      numericInput(ns("window_length"), "Window length", value = 21),
      numericInput(ns("window_interval"), "Window interval", value = 1),
      class = "inputfields_flexbox"
    )
  )
}
    
#' epx_mtw_settings Server Functions
#'
#' @noRd 
mod_epx_mtw_settings_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input[["debug"]], browser())
    
    
    epx_mtw_settings <- reactive({
      list(
        level = input[["level"]],
        factor_cutoff = input[["factor_cutoff"]],
        window_length = input[["window_length"]],
        window_interval = input[["window_interval"]]
      )
    })
    
    return(epx_mtw_settings)
 
  })
}
    
## To be copied in the UI
# mod_epx_mtw_settings_ui("epx_mtw_settings_1")
    
## To be copied in the server
# mod_epx_mtw_settings_server("epx_mtw_settings_1")
