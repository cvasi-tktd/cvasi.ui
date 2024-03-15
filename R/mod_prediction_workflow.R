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
    box(title = span(icon("sliders"), "Model input"), width = 12, status = "primary",
        
        box(title = span(icon("crosshairs"), "Model selection"),        
            status = "primary",
            width = 12,
            div(id = ns("active_model_wrapper"),
            fluidRow(
              column(6,
                     
                         selectInput(ns("active_model"), 
                                     label = "Choose a model",
                                     choices = model_choices)
                     ),
              column(6,
              wellPanel(
                textOutput(ns("model_description"))
              ))))
        ),
        div( id = ns("check_wrapper"), 
             class = "large_hilite",
             dashboardbox_left(title = span(icon("check-square"), "Input check"), 
                 width = 12,
                 uiOutput(ns("complete_params")),
                 uiOutput(ns("complete_init")),
                 uiOutput(ns("complete_forcings")),
                 uiOutput(ns("complete_exposure"))
             )),
        div( id = ns("parameters_wrapper"), 
             class = "large_hilite",
             dashboardbox_left(title = span(icon("cogs"),"Parameters"),
                 status = "primary",
                 width = 12,
                 collapsed = TRUE, 
                 collapsible = TRUE,
                 mod_parameter_input_ui(ns("para_input"))
             )),
        div( id = ns("init_wrapper"), 
             class = "large_hilite",
             dashboardbox_left(title = span(icon("flag"), "Initial values"), 
                 status = "primary",
                 width = 12,
                 collapsed = TRUE, 
                 collapsible = TRUE,
                 mod_init_input_ui(ns("init_input"))
             )),
        div(id = ns("forcings_wrapper"), 
            class = "large_hilite",
            dashboardbox_left(title = span(icon("thermometer-full"),"Forcings"),
                status = "primary",
                width = 12,
                collapsed = TRUE, 
                collapsible = TRUE,
                mod_forcings_input_ui(ns("forcings_input"))
            )),
        div(id = ns("exposure_wrapper"), 
            class = "large_hilite",
            dashboardbox_left(title = div(id = ns("exposure_box_title"), span(icon("shower"), "Exposure")),
                status = "primary",
                width = 12,
                collapsed = TRUE, 
                collapsible = TRUE,
                mod_exposure_input_ui(ns("exposure_input"))
            ))
    ),
    dashboardbox_left(title = div(id = ns("output_box_title"),span(icon("calculator"), "Model output")),
        status = "primary",
        width = 12, 
        collapsed = TRUE, 
        collapsible = TRUE,
        mod_prediction_ui(ns("prediction"))
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
    all_model_dat <- reactiveValues()
    selected_model <- reactiveVal()
    exposure_time_series <- reactiveVal(default_exposure)
    forcings_time_series <- reactiveVal()
    
    # pre-construct all models ----
    # commment: it is not necessary to pre-construct all models. 
    #  Rather only construct the model if selected
    lapply(model_choices, function(x){
      all_model_dat[[x]] <- x %>%
        construct_model() %>% 
        cvasi::set_param(do.call(c, parameter_defaults[[x]]))
    })
    
    
    observeEvent(selected_model(), {
      all_model_dat[[input[["active_model"]]]] <- selected_model()
    })
    
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    

    # Active model change ----------------------------------------------------
    observeEvent(input[["active_model"]],{
      
      shinyjs::html("model_description", {
        paste0("<b>",input[["active_model"]],"</b><br>",model_descriptions[[input[["active_model"]]]])
        })
      
      ## Set selected model -------------------------------------------------
      selected_model(
        all_model_dat[[input[["active_model"]]]]
      )
      
      
      ## Display forcings ----------------------------------------------------
      if (forcings_required(selected_model())){
        shinyjs::show(id = "forcings_box", asis = TRUE)
      } else {
        shinyjs::hide(id = "forcings_box", asis = TRUE)
      }
      
    }
    
    )
    output[["selected_model"]] <- renderPrint(selected_model())
    
    
    # check input data ----
    ## check parameters ----
    output[["complete_params"]] <- renderUI({
      create_valuebox(value = check_model_complete(selected_model()),
                subtitle = "Parameters",
                width = 3)
    })
    
    ## check init values ----
    output[["complete_init"]] <- renderUI({
      create_valuebox(value = check_model_complete(selected_model(), type = "init"),
                subtitle = "Initial values",
                width = 3)
    })
    
    ## check forcings values ----
    output[["complete_forcings"]] <- renderUI({
      if (forcings_required(selected_model())){
        create_valuebox(value = check_forcings_complete(
          expected_forcings = get_required(selected_model(), "forcings"),
          forcings = forcings_time_series()
        ),
        subtitle = "Forcings",
        width = 3)
      } else {
        return(NULL)
      }
    })
    
    ## check exposure ----
    output[["complete_exposure"]] <- renderUI({
      create_valuebox(value = check_exposure_complete(exposure_time_series()),
                subtitle = "Exposure",
                width = 3)
    })
    
    
    # Parameter module server ----
    mod_parameter_input_server("para_input", selected_model)
    
    # Init module server ----
    mod_init_input_server("init_input", selected_model)

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
