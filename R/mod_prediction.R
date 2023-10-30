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
    plotOutput(ns("stat_var_plot")),
    plotOutput(ns("dr_plot"))
  )
}
    
#' prediction Server Functions
#'
#' @noRd 
mod_prediction_server <- function(id, modeldat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    
    sim_result <- reactiveVal()
    
    observeEvent(input[["predict"]],{
      #browser()
      sim_result(
        list(
          stat_var = modeldat() %>% 
            simulate() %>% 
            tidyr::pivot_longer(!matches("time"), names_to = "state_variables"),
          dr = modeldat() %>% 
            dose_response()
        )
        )
      #browser()
    })
    
    output[["stat_var_plot"]] <- renderPlot({
      req(length(sim_result()[["stat_var"]]) > 0)
      ggplot2::ggplot(data = sim_result()[["stat_var"]],
                      ggplot2::aes(x = .data[["time"]], 
                                   y = .data[["value"]], 
                                   color = .data[["state_variables"]]
                      )
      ) + 
        ggplot2::geom_point() +
        ggplot2::geom_line()
    })
    
    output[["dr_plot"]] <- renderPlot({
      req(length(sim_result()[["dr"]]) > 0)
      ggplot2::ggplot(data = sim_result()[["dr"]],
                      ggplot2::aes(x = .data[["mf"]], 
                                   y = .data[["effect"]], 
                                   color = .data[["endpoint"]]
                      )
      ) + 
        ggplot2::geom_point() +
        ggplot2::geom_line()
    })
    
 
  })
}
    
## To be copied in the UI
# mod_prediction_ui("prediction_1")
    
## To be copied in the server
# mod_prediction_server("prediction_1")
