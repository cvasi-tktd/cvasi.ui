#' exposure_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_exposure_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        radioButtons(inputId = ns("exposure_source"), 
                     label = "Source",
                     choices = list(Table = "table",
                                    `Custom file` = "custom",
                                    `TOXSWA file` = "toxswa"
                     )
        ),
        width = 3
      ),
      column(
        textOutput(ns("input_description")),
        width = 9 
      )
    ),# end of fluidRow
    fluidRow(
      uiOutput(ns("input_mod"))
    )
  )
}


#' exposure_input Server Functions
#'
#' @noRd 
mod_exposure_input_server <- function(id, modeldat, exposure_time_series){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    

    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    
    # input modules for the chosen input source --------------------------------
    output[["input_mod"]] <- renderUI({
      if (input[["exposure_source"]] == "table"){
        mod_exposuretable_input_ui(ns("exposuretable_input"))
      }else if (input[["exposure_source"]] == "custom"){
        mod_exposurefile_input_ui(ns("exposurefile_input"))
      } else if (input[["exposure_source"]] == "toxswa"){
        mod_exposuretoxswa_input_ui(ns("exposuretoxswa_input"))
      } else {
        NULL
      }
    })
    
    
    # explanation text for the chosen input source ------------------------------
    observeEvent(input[["exposure_source"]],{
      if (input[["exposure_source"]] == "custom"){
        custom_file_help_text <- "The file should have at least two columns with
        the header 'time' and 'conc'. An optional third column with header 'trial'
        could be added, if several trials are used." %>% 
          div(class="well") %>% 
          as.character()
      } else {
        custom_file_help_text <- ""
      }
      
      shinyjs::html(
        "input_description",
        html = custom_file_help_text
      )
    })
    
    
    # Module servers -----------------------------------------------------------
    ## Exposure table input module server --------------------------------------
    mod_exposuretable_input_server("exposuretable_input", modeldat, exposure_time_series)
    
    ## Exposure file input module server ---------------------------------------
    mod_exposurefile_input_server("exposurefile_input", exposure_time_series)
    
    ## Exposure toxswa input module server -------------------------------------
    mod_exposuretoxswa_input_server("exposuretoxswa_input", exposure_time_series)

  })
}
    
## To be copied in the UI
# mod_exposure_input_ui("exposure_input_1")
    
## To be copied in the server
# mod_exposure_input_server("exposure_input_1")
