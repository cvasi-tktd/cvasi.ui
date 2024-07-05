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
    shiny::fluidRow(
      column(6,
             shiny::textInput(ns("spec_name"), "Species")
      )
    ),
    mod_input_fields_ui(ns("parameter_input_fields")),
    actionButton(ns("assign"), "Assign values"),
    uiOutput(ns("changed_text"))
  )
}
    
#' model_input Server Functions
#'
#' @global .
#' @noRd 
mod_parameter_input_server <- function(id, selected_model, dat_in = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    
    # Render input fields ------------------------------------------------------
    par_vals <- mod_input_fields_server("parameter_input_fields",
                            modeldat = selected_model,
                            type = "param",
                            dat_in)

    # Check change of values and show in GUI -----------------------------------
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
                              class = "input-change", 
                              condition = par_vals_differ())
    })
    
    # Assign values button observer --------------------------------------------
    observeEvent(input[["assign"]], {
      pv <- lapply(rvtl(par_vals), function(x)x()) %>% 
        purrr::discard(is.null)

      selected_model(
        selected_model() %>%
          set_param(pv) %>% 
          set_tag(input$spec_name)
      )
        
    }, ignoreInit = TRUE)
    

  })
}
    
## To be copied in the UI
# mod_parameter_input_ui("main_model_input_1")
    
## To be copied in the server
# mod_parameter_input_server("main_model_input_1")
