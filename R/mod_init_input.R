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
        mod_input_fields_ui(ns("init_input_fields")),
        actionButton(ns("assign"), "Assign values"),
        uiOutput(ns("changed_text"))
        )
}
    
#' model_input Server Functions
#'
#' @global .
#' @noRd 
mod_init_input_server <- function(id, selected_model){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    
    # Render input fields ------------------------------------------------------
    init_vals <- mod_input_fields_server("init_input_fields",
                            modeldat = selected_model,
                            type = "init")
    
    # Check change of values and show in GUI -----------------------------------
    init_vals_differ <- reactive({
      init <- lapply(rvtl(init_vals), function(x)x()) %>% 
        purrr::discard(is.null) %>% 
        .[get_required(selected_model(), "init")]
      md5_stored <- digest::digest(as.numeric(selected_model()@init[names(init)]), algo="md5")
      md5_changed <- digest::digest(as.numeric(do.call(c,init)), algo="md5")
      return(md5_stored != md5_changed)
    })
    
    observeEvent(init_vals_differ(), {
      shinyjs::toggleCssClass(id = "assign", 
                              class = "input-change", 
                              condition = init_vals_differ())
    })
    
    # Assign values button observer --------------------------------------------
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
