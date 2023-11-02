#' parameter_input_fields UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_fields_ui <- function(id){
  ns <- NS(id)
  tagList(
    #actionButton(ns("debug"), "debug"),
    uiOutput(ns("all_fields"))
    
  )
}
    
#' parameter_input_fields Server Functions
#'
#' @noRd 
mod_input_fields_server <- function(id, modeldat, type = "param"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    input_field_vals <- reactiveValues()
    
    # Parameters ----
    observeEvent(modeldat(),{
      parameter_names <- modeldat() %>% 
        get_required(type = type)

      
      lapply(parameter_names,
             function(parname_i){
               field_return <- mod_auto_input_field_server(
                 id = parname_i, 
                 label = parname_i, 
                 value = modeldat() %>% 
                   slot(type) %>% 
                   .[[parname_i]],
                 datatype = "numerical"
               )
               #browser()
               input_field_vals[[parname_i]] <- field_return
             })
    }) # end of observeEvent
    
    output[["all_fields"]] <- renderUI({
      parameter_names <- modeldat() %>% 
        get_required(type = type)#slot("param.req")
      inputFields <- lapply(parameter_names, function(parname_i){
        mod_auto_input_field_ui(ns(parname_i))
      }
      )
      
      tags$div(
        do.call(tagList, inputFields),
        class = "inputfields_flexbox"
      )
    })
    
    
    
    # observeEvent(input[["assign"]], {
    #   vals <- lapply(rvtl(input_field_vals), function(x)x())
    #   if (type == "param"){
    #     modeldat(
    #       modeldat() %>% 
    #         set_param(vals)
    #     )
    #   } else if (type == "init"){
    #     modeldat(
    #       modeldat() %>% 
    #         set_init(vals)
    #     )
    #   }
    #   
    # })
    #parameter_values <- reactive()
    
    return(input_field_vals)
    
  })
}
    
## To be copied in the UI
# mod_input_fields_ui("parameter_input_fields_1")
    
## To be copied in the server
# mod_input_fields_server("parameter_input_fields_1")
