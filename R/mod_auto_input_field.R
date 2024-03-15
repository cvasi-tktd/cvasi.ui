#' auto_input_field UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_auto_input_field_ui <- function(id){
  ns <- NS(id)
  tagList(
    #actionButton(ns("debug"), "debug"),
    uiOutput(ns("inputField"))
  )
}
    
#' auto_input_field Server Functions
#'
#' @noRd 
mod_auto_input_field_server <- function(id,
                                        label,
                                        value,
                                        datatype = NA,
                                        selected = NA,
                                        choices = NA
                                        ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    
    # input field depending on the datatype ----
    output[["inputField"]] <- renderUI({
      tags$div(
        tagList(
          deactivateMouseWheel(),
          
          switch(datatype,
                 
                 logical = checkboxInput(
                   inputId = ns(id),
                   label = label,
                   value = value
                 ),
                 
                 numerical = numericInput(
                   inputId = ns(id),
                   label = label,
                   value = value
                 ),
                 
                 text = textInput(
                   inputId = ns(id),
                   label = label,
                   value = value
                 ),
                 
                 dropdown = selectInput(
                   inputId = ns(id),
                   label = label,
                   selected = value,
                   choices = choices
                 ),
                 
                 textInput(
                   inputId = ns(id),
                   label = label,
                   value = value
                 )
          )
        ))
    }) # end of renderUI
    
    
    return(reactive(input[[id]]))
    
    
  })
}
    
## To be copied in the UI
# mod_auto_input_field_ui("auto_input_field_1")
    
## To be copied in the server
# mod_auto_input_field_server("auto_input_field_1")
