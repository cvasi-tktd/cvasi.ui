#' model_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_model_input_ui <- function(id){
  ns <- NS(id)
  
  tagList(
        actionButton(ns("assign"), "Assign values"),
        tags$h2("Parameters"),
        mod_input_fields_ui(ns("parameter_input_fields")),
        tags$h2("Init"),
        mod_input_fields_ui(ns("init_input_fields"))
        )
}
    
#' model_input Server Functions
#'
#' @noRd 
mod_model_input_server <- function(id, selected_model){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #selected_model <- reactiveVal()
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    

    par_vals <- mod_input_fields_server("parameter_input_fields",
                            modeldat = selected_model,
                            type = "param")
    init_vals <- mod_input_fields_server("init_input_fields",
                            modeldat = selected_model,
                            type = "init")
    
    
    observeEvent(input[["assign"]], {
      pv <- lapply(rvtl(par_vals), function(x)x())
      iv <- lapply(rvtl(init_vals), function(x)x())
      
      selected_model(
        selected_model() %>%
          set_param(pv) %>% 
          set_init(iv)
      )
        
      # if (type == "param"){
      #   selected_model(
      #     selected_model() %>%
      #       set_param(vals)
      #   )
      # } else if (type == "init"){
      #   selected_model(
      #     selected_model() %>%
      #       set_init(vals)
      #   )
      # }

    }, ignoreInit = TRUE)
    

  })
}
    
## To be copied in the UI
# mod_model_input_ui("main_model_input_1")
    
## To be copied in the server
# mod_model_input_server("main_model_input_1")
