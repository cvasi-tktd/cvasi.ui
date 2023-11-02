#' exposure_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_exposure_input_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(4,
           #actionButton(ns("debug"), "debug"),
           actionButton(ns("assign"), "Assign values"),
           rhandsontable::rHandsontableOutput(ns("exposure_table"), width = "100%", height = "100%")
           ),
    column(8,
           tagList(
             plotOutput(ns("exposure_plot"))#,
             #verbatimTextOutput(ns("table_data_print"))  
           )
           )
    
  )
}
    
#' exposure_input Server Functions
#'
#' @noRd 
mod_exposure_input_server <- function(id, modeldat, exposure_time_series){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    

    
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    
    
    # Render the exposure table ----
    output[["exposure_table"]] <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        default_exposure
      )
    })
    
    # 
    exposure_table <- reactive({
      rhandsontable::hot_to_r(input[["exposure_table"]])
      })

    output[["exposure_plot"]] <- renderPlot({
      req(length(exposure_table()) > 0)
      
      ggplot2::ggplot(exposure_table()) + 
        ggplot2::geom_area(ggplot2::aes(time,conc), alpha = 0.75) + 
        #ggplot2::facet_grid(ggplot2::vars(trial))
        ggplot2::facet_wrap(trial ~ ., ncol = 2)

    })
    
    
    observeEvent(input[["assign"]], {
      val <- exposure_table()
      exposure_time_series(val)
      # modeldat(
      #   modeldat() %>% 
      #     set_exposure(val)
      # )
    })
    
  })
}
    
## To be copied in the UI
# mod_exposure_input_ui("exposure_input_1")
    
## To be copied in the server
# mod_exposure_input_server("exposure_input_1")
