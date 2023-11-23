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
    actionButton(ns("debug"), "debug"),
    tags$h2("Forcings"),
    textOutput(ns("req_f")),
    actionButton(ns("set_forcings"), "set forcings"),
    uiOutput(ns("f_input"))
  )
}
    
#' forcings_input Server Functions
#'
#' @noRd 
mod_forcings_input_server <- function(id, selected_model, forcings_time_series){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input[["debug"]], browser())
    

    model_name <- reactive(get_model_name(selected_model()))
    req_f <- reactive(
      get_required(selected_model(), "forcings")
      )
    
    output[["req_f"]] <- renderText(req_f())
    
    output[["f_input"]] <- renderUI({
      model_name()
      print("model name changed")
      req_forcings <- isolate(req_f())
      req( length(req_forcings) > 0 )
      
      do.call(tagList,
              lapply(req_forcings, function(f_name) {
                f_id <- ns(f_name)
                numericInput(inputId = f_id,
                             label = f_name,
                             value = 0
                             )
                }
                )
              )
      
      
    })
    
    
    observeEvent(input[["set_forcings"]], {
      req( length(req_f()) > 0 )

      input_f_vals <- lapply(setNames(req_f(), req_f()), function(f_name) {
        #f_id <- ns(f_name)
        out <- data.frame(0, input[[f_name]])
        colnames(out) <- c("t", f_name)
        out
      })

      forcings_time_series(input_f_vals)

    })


    
  })
}
    
## To be copied in the UI
# mod_forcings_input_ui("forcings_input_1")
    
## To be copied in the server
# mod_forcings_input_server("forcings_input_1")
