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
      uiOutput(ns("stat_plot_UI")),
      tags$hr(),
      uiOutput(ns("epx_plot_UI"))
    )
  )
  
}

#' prediction Server Functions
#'
#' @global time conc
#' @importFrom grDevices axisTicks
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
    observeEvent(forcings_time_series[[modeldat() 
                                         %>% class() %>% 
                                           lookup_name()]],
                 {
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
                     set_forcings(forcings_time_series[[modeldat() 
                                                        %>% class() %>% 
                                                          lookup_name()]]
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
              set_forcings(forcings_time_series[[modeldat() 
                                                 %>% class() %>% 
                                                   lookup_name()]]
              )
          }

          sim_result[["epx_mtw"]] <- model_input %>% 
            epx_mtw(level = epx_mtw_settings()[["level"]],#10, 
                    factor_cutoff = epx_mtw_settings()[["factor_cutoff"]],#1000,
                    window_length = epx_mtw_settings()[["window_length"]],#7, 
                    window_interval = epx_mtw_settings()[["window_interval"]]#1)
            ) %>% 
            dplyr::mutate(endpoint = dplyr::case_when(
              endpoint %in% c("BM","A") ~ "biomass",
              endpoint %in% c("r") ~ "growth rate",
            ))
          
          
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
        dashboardbox_left(
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
                nint = 5),
              y_title = "biomass"
      ) + 
        ggplot2::theme(axis.text = ggplot2::element_text(size = 13),
                       axis.title = ggplot2::element_text(size = 14),
                       strip.text = ggplot2::element_text(size = 14)
        ) +
        ggplot2::scale_color_manual(name = "",
                                    labels = "predicted biomass",
                                    values="black") +
        ggplot2::scale_fill_manual(name = "",
                                   labels = "exposure pattern",
                                   values="black") +
        ggplot2::theme(legend.position = "right",
                       # legend.spacing.y = ggplot2::unit(-1.2, "cm"),
                       legend.text = ggplot2::element_text(size = 13))
    })
    
    ## EPx results -------------------------------------------------------------
    output[["epx_mtw_plot"]] <- renderPlot({
      req(length(sim_result[["epx_mtw"]]) > 0)
      exposure <- isolate(exposure_time_series()[,c("time","conc")])
      
      sim_result[["epx_mtw"]] %>% 
      plot_epx(exposure_ts = exposure, conc_y_title = "env. concentration [µg/L]")
      
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
    
    # Plot panels ----
    output[["stat_plot_UI"]] <- renderUI({
      req(req(length(sim_result[["stat_var"]]) > 0))
      
      tagList(
        h4("Prediction results"),
        div(
          plotOutput(ns("stat_var_plot"), width = "100%", height = "600px", fill = FALSE), 
          style = "max-width: 800px;"
        )
      )
    })
    
    output[["epx_plot_UI"]] <- renderUI({
      req(sim_result[["epx_mtw"]])
      tagList(
        h4("EPx results"),
        div(
          HTML("Summary table of worst case windows per endpoint",
               as.character(
                 actionLink(inputId=ns("info_button_summarytable"),
                            label="",
                            icon=icon("info-circle"),
                            class="info-circle-link"
                 )
               )),
          DT::dataTableOutput(ns("epx_mtw_summary_table")),
          div(
            HTML("Plot of EPx values in moving time windows",
                 as.character(
                   actionLink(inputId=ns("info_button_epx"),
                              label="",
                              icon=icon("info-circle"),
                              class="info-circle-link"
                   )
                 )),
            style = "padding-top: 30px;"),
          plotOutput(ns("epx_mtw_plot"), width = "100%", height = "600px", fill = FALSE), 
          style = "max-width: 800px;"
        )
      )
      
    })
    
    ## Info section - plot panels ----
    ### observers ----
    observeEvent(input[["info_button_summarytable"]], {
      showModal(modalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3("Info summary table"),
                                "<br>",
                                "The table summarizes the EPx values calculated
                                for all windows of the moving time windows. In 
                                detail \"moving time window\" approach means that
                                the whole exposure profile is devided into smaller
                                overlapping segments and for each segment the EPx 
                                is calculated.<br>
                                This table shows the windows that yield the lowest
                                EPx of all windows for each relevant endpoint."
                              )
                            ),
                            footer=tagList(NULL),
                            easyClose=TRUE)
      )
    })
    
    observeEvent(input[["info_button_epx"]], {
            showModal(modalDialog(title = NULL,
                                  HTML(
                                    paste0(
                                      tags$h3("Info EPx plot"),
                                      "<br>",
                                      "The top panel shows the exposure 
                                      concentration given for the exposure 
                                      profile with a blue shaded area. The bottom
                                      panel shows the corresponding EPx (solid 
                                      orange line) calculated for the time window
                                      starting at the corresponding point on the
                                      x-axis. The <b>EPx</b> is the effective 
                                      multiplication factor by which the profile 
                                      needs to be multiplied to achieve a x% 
                                      effect on the selected endpoint at the 
                                      end of the window."
                                    )
                                  ),
                                  footer=tagList(NULL),
                                  easyClose=TRUE)
      )
    })
    
    
    
    
  })
}

## To be copied in the UI
# mod_prediction_ui("prediction_1")
    
## To be copied in the server
# mod_prediction_server("prediction_1")
