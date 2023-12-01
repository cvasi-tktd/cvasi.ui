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
        tags$hr(),
        tags$h2("Init"),
        mod_input_fields_ui(ns("init_input_fields")),
        uiOutput(ns("forcings_ui"))
        )
}
    
#' model_input Server Functions
#'
#' @noRd 
mod_model_input_server <- function(id, selected_model){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


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
      pv <- lapply(rvtl(par_vals), function(x)x()) %>% 
        purrr::discard(is.null)
      iv <- lapply(rvtl(init_vals), function(x)x())%>% 
        purrr::discard(is.null)
      
      selected_model(
        selected_model() %>%
          set_param(pv) %>% 
          set_init(iv)
      )
        
    }, ignoreInit = TRUE)
    

  })
}
    
## To be copied in the UI
# mod_model_input_ui("main_model_input_1")
    
## To be copied in the server
# mod_model_input_server("main_model_input_1")
