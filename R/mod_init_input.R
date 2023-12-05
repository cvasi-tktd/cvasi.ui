#' model_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_init_input_ui <- function(id){
  ns <- NS(id)
  
  tagList(
        actionButton(ns("assign"), "Assign values"),
        mod_input_fields_ui(ns("init_input_fields"))
        )
}
    
#' model_input Server Functions
#'
#' @noRd 
mod_init_input_server <- function(id, selected_model){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    

    init_vals <- mod_input_fields_server("init_input_fields",
                            modeldat = selected_model,
                            type = "init")
    

    observeEvent(input[["assign"]], {
      iv <- lapply(rvtl(init_vals), function(x)x())%>% 
        purrr::discard(is.null)
      
      selected_model(
        selected_model() %>%
          set_init(iv)
      )
        
    }, ignoreInit = TRUE)
    

  })
}
    
## To be copied in the UI
# mod_init_input_ui("main_model_input_1")
    
## To be copied in the server
# mod_init_input_server("main_model_input_1")
