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
    #actionButton(ns("debug"), "debug"),
    actionButton(ns("predict"), "Predict"),
    #tags$div(textOutput(ns("error_text")), class = "text-danger"),
    
    textOutput(ns("error_text_sv")),
    plotOutput(ns("stat_var_plot"), width = "100%", height = "600px", fill = FALSE)
    #plotly::plotlyOutput(ns("stat_var_plot"))
    
    # fluidRow(
    #   column(6,
    #          tags$h3("State variables"),
    #          textOutput(ns("error_text_sv")),
    #          plotOutput(ns("stat_var_plot"))
    #   ),
    #   column(6,
    #          tags$h3("Dose-response"),
    #          textOutput(ns("error_text_dr")),
    #          plotOutput(ns("dr_plot"))
    #   )
    # )# end of fluidRow
    
    
  )  
  
}
    
#' prediction Server Functions
#'
#' @noRd 
mod_prediction_server <- function(id, modeldat, exposure_time_series){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    
    #sim_result <- reactiveVal()
    sim_result <- reactiveValues()
    
    observeEvent(modeldat(),{
      sim_result[["stat_var"]] <- NULL
      #sim_result[["dr"]] <- NULL
    })
    
    
    observeEvent(input[["predict"]],{
    #observeEvent(modeldat(),{
      shinybusy::show_modal_spinner(text = "Predicting...")
      tryCatch({
        shinyjs::html("error_text_sv", "")
        sim_result[["stat_var"]] <- NULL

          # sim_result[["stat_var"]] <- modeldat() %>% 
          #   simulate() %>% 
          #   tidyr::pivot_longer(!matches("time"), names_to = "state_variables")
          # 
        #browser()
        sim_result[["stat_var"]] <- simulate_batch(model_base = modeldat(),
                                                   treatments = exposure_time_series(),
                                                   param_sample = NULL)
          
        },

        warning = function(cond) warnings_f(cond, id = "error_text_sv"),

        error = function(cond) error_f(cond, id = "error_text_sv")
      )
      
      # tryCatch({
      #   shinyjs::html("error_text_dr", "")
      #   sim_result[["dr"]] <- NULL
      # 
      #   sim_result[["dr"]] <- modeldat() %>%
      #     dose_response()
      #   },
      #   warning = function(cond) warnings_f(cond, id = "error_text_dr"),
      #   error = function(cond) error_f(cond, id = "error_text_dr")
      # )
      
      shinybusy::remove_modal_spinner()

    }, ignoreInit = TRUE)
    
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

    
    
    output[["stat_var_plot"]] <- renderPlot({#plotly::renderPlotly({#
      req(length(sim_result[["stat_var"]]) > 0)
      
      # ggplot2::ggplot(data = sim_result[["stat_var"]],
      #                 ggplot2::aes(x = .data[["time"]], 
      #                              y = .data[["value"]], 
      #                              color = .data[["state_variables"]]
      #                 )
      # ) + 
      #   ggplot2::geom_point() + 
      #   ggplot2::geom_line()
      #browser()
      plot_sd(model_base = modeldat(),
              treatments = exposure_time_series(),
              rs_mean = sim_result[["stat_var"]],
              obs_mean = NULL) + 
        ggplot2::theme(axis.text = ggplot2::element_text(size = 13),
                       axis.title = ggplot2::element_text(size = 14),
                       strip.text = ggplot2::element_text(size = 14)
                       ) #+
        #ggplot2::theme(axis.title=ggplot2::element_text(size=14)) +
        #ggplot2::theme(legend.text=ggplot2::element_text(size=12))
      
      
    })
    
    # output[["dr_plot"]] <- renderPlot({
    #   req(length(sim_result[["dr"]]) > 0)
    #   ggplot2::ggplot(data = sim_result[["dr"]],
    #                   ggplot2::aes(x = .data[["mf"]], 
    #                                y = .data[["effect"]], 
    #                                color = .data[["endpoint"]]
    #                   )
    #   ) + 
    #     ggplot2::geom_point() +
    #     ggplot2::geom_line()
    # })
    
 
  })
}
    
## To be copied in the UI
# mod_prediction_ui("prediction_1")
    
## To be copied in the server
# mod_prediction_server("prediction_1")
