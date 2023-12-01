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

    fluidRow(
      column(6,
             wellPanel(
               textOutput(ns("model_description"))
             )
             ),
      column(6,
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
      ),
      tabPanel("Forcings",
               mod_forcings_input_ui(ns("forcings_input"))
      ),
      tabPanel("Exposure", 
               mod_exposure_input_ui(ns("exposure_input"))
               ),
      tabPanel("Prediction", 
               mod_prediction_ui(ns("prediction"))
               ),
      id = ns("iotabpanel")
    )
    
  )
}

#' prediction_workflow Server Functions
#'
#' @noRd 
mod_prediction_workflow_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # reactiveVal(ues) definitions ----
    selected_model <- reactiveVal()
    all_model_dat <- reactiveValues()
    exposure_time_series <- reactiveVal(default_exposure)
    forcings_time_series <- reactiveVal()
    
    # pre-construct all models ----
    # commment: it is not necessary to pre-construct all models. 
    #  Rather only construct the model if selected
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
      
      # Forcings tab to show ####
      if (forcings_required(selected_model())){
        showTab( inputId = "iotabpanel", target = "Forcings")
      } else {
        hideTab( inputId = "iotabpanel", target = "Forcings")
        
      }
      
    }
    
    )
    output[["selected_model"]] <- renderPrint(selected_model())
    
    
    # check input data ----
    ## check parameters ----
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
    
    ## check init values ----
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
    })
    
    ## check forcings values ----
    output[["complete_forcings"]] <- renderUI({
      if (forcings_required(selected_model())){
        
        if (check_forcings_complete(expected_forcings = get_required(selected_model(), "forcings"),
                                    forcings = forcings_time_series()
                                    ))
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
      } else {
        return(NULL)
      }
    })
    
    ## check exposure ----
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
    })
    
    
    # Parameter and init module server ----
    mod_model_input_server("para_init", selected_model)

    # Forcing input module server ----
    mod_forcings_input_server("forcings_input", selected_model, forcings_time_series)
    
    # Exposure input module server ----
    mod_exposure_input_server("exposure_input", selected_model, exposure_time_series)

    # Prediction module server ----
    mod_prediction_server("prediction", selected_model, exposure_time_series, forcings_time_series)
    

  })
}


## To be copied in the UI
# mod_prediction_workflow_ui("prediction_workflow_1")
    
## To be copied in the server
# mod_prediction_workflow_server("prediction_workflow_1")
