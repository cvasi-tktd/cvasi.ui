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
    #textOutput(ns("selected_model_name")),
    
    fluidRow(
      column(6,
             #verbatimTextOutput(ns("selected_model"))
             wellPanel(
               textOutput(ns("model_description"))
             )
             ),
      column(6,
             # wellPanel(
             #   textOutput(ns("model_description"))
             #   ),
             tagList(
               tags$h3("Input check"),
               tags$div(
                 uiOutput(ns("complete_params")),
                 uiOutput(ns("complete_init")),
                 uiOutput(ns("complete_forcings")),
                 uiOutput(ns("complete_exposure")),
                 class = "bold"
               )
             )
             )
    ),
    
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
    all_model_dat <- reactiveValues()
    exposure_time_series <- reactiveVal()
    
    lapply(model_choices, function(x){
      all_model_dat[[x]] <- x %>%
        construct_model() %>% 
        neofm::set_param(do.call(c, parameter_defaults[[x]]))
    })
    
    
    observeEvent(selected_model(), {
      all_model_dat[[input[["active_model"]]]] <- selected_model()
    })
    
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    

    observeEvent(input[["active_model"]],{
      shinyjs::html("model_description", {
        paste0("<b>",input[["active_model"]],"</b><br>",model_descriptions[[input[["active_model"]]]])
        })
      selected_model(
        all_model_dat[[input[["active_model"]]]]

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
    
    
    # check input data ----
    output[["complete_params"]] <- renderUI({
      if (check_model_complete(selected_model()))
      {
        set_class <- "text-success"
        set_icon <- fontawesome::fa_i("check")
      } else {
        set_class <- "text-danger"
        set_icon <- fontawesome::fa_i("xmark")
      }
        
      tags$span(
        paste0("Parameters "),
        set_icon,
        class = set_class
      )
    })
    output[["complete_init"]] <- renderUI({
      if (check_model_complete(selected_model(), type = "init"))
      {
        set_class <- "text-success"
        set_icon <- fontawesome::fa_i("check")
      } else {
        set_class <- "text-danger"
        set_icon <- fontawesome::fa_i("xmark")
      }
      tags$span(
        paste0("Initial values"),
        set_icon,
        class = set_class
      )
      #paste0("Initial values")#: ", check_model_complete(selected_model(), "init"))
    })
    output[["complete_forcings"]] <- renderUI({
      if (check_model_complete(selected_model(), type = "forcings"))
      {
        set_class <- "text-success"
        set_icon <- fontawesome::fa_i("check")
      } else {
        set_class <- "text-danger"
        set_icon <- fontawesome::fa_i("xmark")
      }
      tags$span(
        paste0("Forcings"),
        set_icon,
        class = set_class
      )
    })
    
    output[["complete_exposure"]] <- renderUI({
      if (check_exposure_complete(exposure_time_series()))
      {
        set_class <- "text-success"
        set_icon <- fontawesome::fa_i("check")
      } else {
        set_class <- "text-danger"
        set_icon <- fontawesome::fa_i("xmark")
      }
      tags$span(
        paste0("Exposure"),
        set_icon,
        class = set_class
      )
      
      
     # paste0("Exposure")#: ", check_exposure_complete(exposure_time_series()))
    })
    
    
    
    mod_model_input_server("para_init", selected_model)

    mod_exposure_input_server("exposure_input", selected_model, exposure_time_series)

    mod_prediction_server("prediction", selected_model, exposure_time_series)
    
    
  })
}


## To be copied in the UI
# mod_prediction_workflow_ui("prediction_workflow_1")
    
## To be copied in the server
# mod_prediction_workflow_server("prediction_workflow_1")
