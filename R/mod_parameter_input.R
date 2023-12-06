#' model_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_parameter_input_ui <- function(id){
  ns <- NS(id)
  
  tagList(
        actionButton(ns("assign"), "Assign values"),
        uiOutput(ns("changed_text")),
        tags$h2("Parameters"),
        mod_input_fields_ui(ns("parameter_input_fields")),
        )
}
    
#' model_input Server Functions
#'
#' @noRd 
mod_parameter_input_server <- function(id, selected_model){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    

    par_vals <- mod_input_fields_server("parameter_input_fields",
                            modeldat = selected_model,
                            type = "param")

    
    par_vals_differ <- reactive({
      req(rvtl(par_vals), selected_model()@param)
      pv <- lapply(rvtl(par_vals), function(x)x()) %>% 
        purrr::discard(is.null) %>% 
        .[get_required(selected_model(), "param")]
      md5_stored <- digest::digest(do.call(cbind,selected_model()@param[names(pv)]), algo="md5")
      md5_changed <- digest::digest(do.call(cbind,pv), algo="md5")
      return(md5_stored != md5_changed)
    })
    
    observeEvent(par_vals_differ(), {
      shinyjs::toggleCssClass(id = "assign", 
                              class = "yellow-background", 
                              condition = par_vals_differ())
    })
    

    observeEvent(input[["assign"]], {
      pv <- lapply(rvtl(par_vals), function(x)x()) %>% 
        purrr::discard(is.null)

      selected_model(
        selected_model() %>%
          set_param(pv)
      )
        
    }, ignoreInit = TRUE)
    

  })
}
    
## To be copied in the UI
# mod_parameter_input_ui("main_model_input_1")
    
## To be copied in the server
# mod_parameter_input_server("main_model_input_1")
