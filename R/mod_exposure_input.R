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
mod_exposure_input_server <- function(id, modeldat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    default_exposure <- data.frame(
      t=c(0,1,50,100,101,200,201,400),
      #pec=c(0,0,0.1,0.1,0,0))
      conc=c(0,0,0,0,1,0.1,0,0)
    )
    
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    output[["exposure_table"]] <- default_exposure %>% 
      rhandsontable::rhandsontable() %>% 
      rhandsontable::renderRHandsontable()
    
    
    exposure_table <- reactive({
      rhandsontable::hot_to_r(input[["exposure_table"]])
      })
    output[["table_data_print"]] <- renderPrint(exposure_table())
    
    output[["exposure_plot"]] <- renderPlot({
      req(length(exposure_table()) > 0)
      
      ggplot2::ggplot(exposure_table()) + 
        ggplot2::geom_polygon(ggplot2::aes(t,conc))
    })
    
    
    observeEvent(input[["assign"]], {
      #browser()
      val <- exposure_table()
      modeldat(
        modeldat() %>% 
          set_exposure(val)
      )
    })
    
  })
}
    
## To be copied in the UI
# mod_exposure_input_ui("exposure_input_1")
    
## To be copied in the server
# mod_exposure_input_server("exposure_input_1")
