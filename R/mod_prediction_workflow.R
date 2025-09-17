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

    box(title = span(icon("sliders"), "Model input"), width = 12, status = "primary",
        
        box(title = span(icon("crosshairs"), "Model selection"),        
            status = "primary",
            width = 12,
            div(id = ns("active_model_wrapper"),
                fluidRow(
                  column(6,
                         
                         selectInput(ns("active_model"), 
                                     label = "Choose a model",
                                     choices = cvasi.ui::model_choices),
                         div(
                           downloadButton(ns("save"), "Save") %>% 
                             tagAppendAttributes(class = "io-btn", 
                                                 title = "Save parameters and initial values."),
                           fileInputOnlyButton(ns("load"),
                                               label = NULL,
                                               buttonLabel = list(
                                                 icon("upload", class = NULL,lib = "font-awesome"),
                                                 "Load"
                                               )) %>% 
                             shiny::tagAppendAttributes(class = "io-btn", 
                                                        title = "Load parameters and initial values.",
                                                        .cssSelector = ".btn.btn-default"),
                           style = "display: flex; flex-wrap: wrap; justify-content: flex-start; align-items: flex-start;"
                         )
                  ),
                  column(6,
                         wellPanel(
                           textOutput(ns("model_description"))
                         ))))
        ),
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
            dashboardbox_left(title = span(icon("thermometer-full"), "External drivers"),
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
            )),
        div( id = ns("check_wrapper"), 
             class = "large_hilite",
             dashboardbox_left(title = span(icon("check-square"), "Input check"), 
                               width = 12,
                               uiOutput(ns("complete_params")),
                               uiOutput(ns("complete_init")),
                               uiOutput(ns("complete_forcings")),
                               uiOutput(ns("complete_exposure"))
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
    
    observeEvent(input[["debug"]], browser())
    
    # reactiveVal(ues) definitions ----
    all_model_dat <- reactiveValues()
    selected_model <- reactiveVal()
    exposure_time_series <- reactiveVal(cvasi.ui::default_exposure)
    forcings_time_series <- do.call(reactiveValues, lapply(cvasi.ui::model_defaults, function(x) x[["forcing_defaults"]]))
    import_trigger <- reactiveVal(NULL)
    
    # pre-construct all models ----
    # commment: it is not necessary to pre-construct all models. 
    #  Rather only construct the model if selected
    lapply(cvasi.ui::model_choices, function(x){
      all_model_dat[[x]] <- x %>%
        construct_model() %>% 
        cvasi::set_param(do.call(c, cvasi.ui::model_defaults[[x]][["parameter_defaults"]])) %>% 
        cvasi::set_init(do.call(c, cvasi.ui::model_defaults[[x]][["init_defaults"]]))
    })
    
    
    observeEvent(selected_model(), {
      all_model_dat[[input[["active_model"]]]] <- selected_model()
    })
    
    
    # Export data --------------------------------------------------------------
    output$save <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.zip', sep='')
      },
      content = function(con) {
        
        active_model <- input[["active_model"]]
        param <- slot(selected_model(), "param")
        init <- as.list(slot(selected_model(), "init"))
       # browser()
        dat_out <- list(
          active_model = active_model,
          param = param,
          init = init
        )
        pack(dat_out, zipfile = con)
      }
    )
    
    
    # Import data --------------------------------------------------------------
    dat_in <- reactive({
      if(length(input$load))
        unpack(input$load$datapath)
      else
        NULL
    })
    
    observeEvent(dat_in(),{
      req(length(dat_in()))
      active_model <- dat_in()[["active_model"]]
      updateSelectInput(session, "active_model", 
                        selected = active_model)
      
      param <- dat_in()[["param"]]
      init <- dat_in()[["init"]]
      smodel <- active_model %>%
        construct_model() %>% 
        cvasi::set_param(do.call(c, param)) %>% 
        cvasi::set_init(init)
      all_model_dat[[active_model]] <- smodel
      selected_model(smodel)

      import_trigger(date())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    
    # Active model change ----------------------------------------------------
    observeEvent(input[["active_model"]],{
      
      shinyjs::html("model_description", {
        message(input[["active_model"]], " selected")
        #paste0("<b>",input[["active_model"]],"</b><br>",cvasi.ui::model_descriptions[[input[["active_model"]]]])
        paste0("<b>",
               input[["active_model"]],
               "</b><br>",
               read_roxygen(package = "cvasi",
                            f_name = lookup_name(input[["active_model"]], from="model_f", to="model_topic"),
                            tag = "\\description")
               #cvasi.ui::model_descriptions[[]]
        )
      })
      ## Set selected model -------------------------------------------------
      selected_model(
        all_model_dat[[input[["active_model"]]]]
      )

      ## Display forcings ----------------------------------------------------
      if (forcings_required(selected_model())){
        shinyjs::show(id = "prediction_workflow-forcings_wrapper", asis = TRUE)
      } else {
        shinyjs::hide(id = "prediction_workflow-forcings_wrapper", asis = TRUE)
      }
    }
    
    )
    output[["selected_model"]] <- renderPrint(selected_model())
    
    # check input data ----
    ## check parameters ----
    output[["complete_params"]] <- renderUI({
      mod_complete <- check_model_complete(selected_model())
      parameter_range_ok <- all(pars_range(selected_model())$in_range)
      if (!mod_complete)
        val <- FALSE
      else if (mod_complete & !parameter_range_ok)
        val <- "range"
      else
        val <- TRUE
      create_valuebox(value = val,
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
          forcings = rvtl(forcings_time_series)[[selected_model() %>% 
                                                   class() %>% 
                                                   lookup_name()]]
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
    mod_parameter_input_server("para_input", selected_model, dat_in)
    
    # Init module server ----
    mod_init_input_server("init_input", selected_model, dat_in)
    
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
