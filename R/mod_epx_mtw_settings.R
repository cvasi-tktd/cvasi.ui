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
    tags$div(
      numericInput(inputId = ns("level"),
                   label = tooltip_text(
                     "Effect level",
                     tooltip = "The level \"x\" at which the effect is evaluated."
                     ), 
                   value = 50),
      numericInput(inputId = ns("factor_cutoff"), 
                   label = tooltip_text(
                     "Cutoff value",
                     tooltip = "The value above which the EPx is not evaluated exactly."
                     ), 
                   value = 1000),
      numericInput(inputId = ns("window_length"), 
                   label = tooltip_text(
                     "Window length",
                     tooltip = "The length of each moving time window for which the EPx is calculated."
                     ), 
                   value = 21),
      numericInput(inputId = ns("window_interval"), 
                   label = tooltip_text(
                     "Window interval",
                     tooltip = "The time interval a window moves each step."
                     ), 
                   value = 1),
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
