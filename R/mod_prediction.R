#' prediction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_prediction_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("predict"), "Predict", style = "margin-bottom: 1em;"),
    textOutput(ns("error_text_sv")),
    fluidRow(
      div(id = ns("epx_fields_wrapper"), 
          style = "display: inline-block; box-sizing: content-box; width: 100%;",
          uiOutput(ns("epx_fields"))
      )
    ),
    div(
      div(
        plotOutput(ns("stat_var_plot"), width = "100%", height = "600px", fill = FALSE), 
        style = "max-width: 800px;"
      ),
      tags$hr(),
      div(
        DT::dataTableOutput(ns("epx_mtw_summary_table")),
        plotOutput(ns("epx_mtw_plot"), width = "100%", height = "600px", fill = FALSE), 
        style = "max-width: 800px;"
      )
    )
  )
  
}

#' prediction Server Functions
#'
#' @noRd 
mod_prediction_server <- function(id, modeldat, exposure_time_series, forcings_time_series){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    # reactive value definition ------------------------------------------------
    sim_result <- reactiveValues()

    
    # reactives ----------------------------------------------------------------
    req_forcings <- reactive(
      get_required(modeldat(), "forcings")
    )
    
    n_trials <- reactive({
      length(unique(exposure_time_series()[,"trial"]))
      })
    
    
    # observers ----------------------------------------------------------------
    observeEvent(modeldat(),{
      sim_result[["stat_var"]] <- NULL
      sim_result[["epx_mtw"]] <- NULL
    })
    observeEvent(forcings_time_series(),{
      sim_result[["stat_var"]] <- NULL
      sim_result[["epx_mtw"]] <- NULL
    })
    observeEvent(exposure_time_series(),{
      sim_result[["stat_var"]] <- NULL
      sim_result[["epx_mtw"]] <- NULL
    })
    
    
    # Predict button observer -------------------------------------------------
    observeEvent(input[["predict"]],{

      shinybusy::show_modal_spinner(text = "Predicting...")
      
      ## Simulate --------------------------------------------------------------
      tryCatch({
        shinyjs::html("error_text_sv", "")
        sim_result[["stat_var"]] <- NULL

        model_input <- modeldat()
        
        if (length(req_forcings()) > 0){
          model_input <- model_input %>% 
                     set_forcings(forcings_time_series()
          )
        }

        sim_result[["stat_var"]] <- simulate_batch(model_base = model_input,
                                                   treatments = exposure_time_series(),
                                                   param_sample = NULL)
          
        },
        error = function(cond) error_f(cond, id = "error_text_sv")
      )
      
      # Calculate EPx ----------------------------------------------------------
      shinybusy::update_modal_spinner(text = "Calculating EPx in moving time windows...")

      if ( n_trials() == 1){
        tryCatch({
          shinyjs::html("error_text_sv", "")
          sim_result[["epx_mtw"]] <- NULL

          model_input <- modeldat() %>% 
            set_exposure(exposure_time_series() %>% dplyr::select(time,conc))
          
          if (length(req_forcings()) > 0){
            model_input <- model_input %>% 
              set_forcings(forcings_time_series()
              )
          }

          sim_result[["epx_mtw"]] <- model_input %>% 
            cvasi:::epx_mtw(level = epx_mtw_settings()[["level"]],#10, 
                    factor_cutoff = epx_mtw_settings()[["factor_cutoff"]],#1000,
                    window_length = epx_mtw_settings()[["window_length"]],#7, 
                    window_interval = epx_mtw_settings()[["window_interval"]]#1)
            )
          
          
        },
        error = function(cond) error_f(cond, id = "error_text_sv")
        )
      }
      
      
      shinybusy::remove_modal_spinner()

    }, ignoreInit = TRUE)
    
    # Warning/Error output functions -------------------------------------------
    warnings_f <- function(cond, id){
      shinyjs::html(id = id,
                    html = paste0("<div class = \"text-warning\">",as.character(cond),"</div>"),
                    add = TRUE)
    }
    error_f <- function(cond, id){
      shinyjs::html(id = id, 
                    html = paste0("<div class = \"alert alert-danger\">",as.character(cond),"</div>"),
                    add = TRUE)
    }

    # Render EPx fields for setting input --------------------------------------
    output[["epx_fields"]] <- renderUI({
      if ( n_trials() == 1){
        shinydashboard::box(
          mod_epx_mtw_settings_ui(ns("epx_mtw_settings_1")),
          title = "EPx and moving time window settings",
          collapsible = TRUE, 
          width = 12
        )
        } else {
          NULL
        }
      })
    epx_mtw_settings <- mod_epx_mtw_settings_server("epx_mtw_settings_1")
    
    # Render plots -------------------------------------------------------------
    ## Simulation results ------------------------------------------------------
    output[["stat_var_plot"]] <- renderPlot({
      req(length(sim_result[["stat_var"]]) > 0)
      
      plot_sd(model_base = isolate(modeldat()),
              treatments = isolate(exposure_time_series()),
              rs_mean = sim_result[["stat_var"]],
              obs_mean = NULL,
              x_breaks = axisTicks(
                usr = range(isolate(exposure_time_series())[,"time"]),
                log = FALSE, 
                axp = NULL,
                nint = 5)
              ) + 
        ggplot2::theme(axis.text = ggplot2::element_text(size = 13),
                       axis.title = ggplot2::element_text(size = 14),
                       strip.text = ggplot2::element_text(size = 14)
                       ) #+
        #ggplot2::theme(axis.title=ggplot2::element_text(size=14)) +
        #ggplot2::theme(legend.text=ggplot2::element_text(size=12))

    })
    
    ## EPx results -------------------------------------------------------------
    output[["epx_mtw_plot"]] <- renderPlot({
      req(length(sim_result[["epx_mtw"]]) > 0)
      exposure <- isolate(exposure_time_series()[,c("time","conc")])
      plot_epx(EPx_ts = sim_result[["epx_mtw"]],
               exposure_ts = exposure)
    })
    
    epx_mtw_summary <- reactive({
      req(sim_result[["epx_mtw"]])
      epx_summary <- cvasi:::epx_min_win(sim_result[["epx_mtw"]])
      epx_summary
    })
    output[["epx_mtw_summary"]] <- renderPrint(epx_mtw_summary())
    output[["epx_mtw_summary_table"]] <- DT::renderDataTable({
      epx_mtw_summary() %>%
        dplyr::rename_all(~ gsub("\\.", " ", .)) %>%
        dplyr::rename_all(~ gsub("^(.)", "\\U\\1", ., perl = TRUE)) %>% 
        DT::datatable(rownames = FALSE,
                      filter = "none",
                      selection = "none",
                      options = list(
                        dom = "t",
                        scrollX = TRUE
                      ))%>% 
        DT::formatRound(columns=c('EPx'), digits=3)
    })
    
    
    
  })
}

## To be copied in the UI
# mod_prediction_ui("prediction_1")
    
## To be copied in the server
# mod_prediction_server("prediction_1")
