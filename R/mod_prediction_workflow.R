#' prediction_workflow UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_prediction_workflow_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    #actionButton(ns("debug"), "debug"),
    selectInput(ns("active_model"), 
                label = "Choose a model",
                choices = model_choices),
    textOutput(ns("selected_model_name")),
    verbatimTextOutput(ns("selected_model")),
    
    tabsetPanel(
      tabPanel("Parameters/Init", 
               mod_model_input_ui(ns("para_init"))
               # tagList(
               #   tags$h2("Parameters"),
               #   mod_input_fields_ui(ns("parameter_input_fields")),
               #   tags$h2("Init"),
               #   mod_input_fields_ui(ns("init_input_fields"))
               # )
      ),
      
      tabPanel("Exposure", 
               mod_exposure_input_ui(ns("exposure_input"))
               ),
      
      tabPanel("Prediction", 
               mod_prediction_ui(ns("prediction"))
               )
    )
    
  )
}

#' prediction_workflow Server Functions
#'
#' @noRd 
mod_prediction_workflow_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    selected_model <- reactiveVal()
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    
    output[["selected_model_name"]] <- renderText(input[["active_model"]])
    
    
    observeEvent(input[["active_model"]],{
      selected_model(
        input[["active_model"]] %>%
          construct_model() %>% 
          neofm::set_param(do.call(c, parameter_defaults[[input[["active_model"]]]]))
      )
      
      # hard coded setting of forcings for testing; include user input for the forcings and remove later!
      if (forcings_required(selected_model())){
        f <- forcing_defaults[[input[["active_model"]]]]
        selected_model(selected_model() %>% 
                         set_forcings(f)
                       )
      }
      
    }
    
    )
    output[["selected_model"]] <- renderPrint(selected_model())
    
    
    
    mod_model_input_server("para_init", selected_model)

    mod_exposure_input_server("exposure_input", selected_model)

    mod_prediction_server("prediction", selected_model)
    
    
  })
}


## To be copied in the UI
# mod_prediction_workflow_ui("prediction_workflow_1")
    
## To be copied in the server
# mod_prediction_workflow_server("prediction_workflow_1")
