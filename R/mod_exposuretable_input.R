#' exposure_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_exposuretable_input_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(4,
           #actionButton(ns("debug"), "debug"),
           actionButton(ns("assign"), "Assign values"),
           rhandsontable::rHandsontableOutput(ns("exposure_table"),
                                              width = "100%",
                                              height = "100%")
           ),
    column(8,
           tagList(
             plotOutput(ns("exposure_plot"))
           )
           )
    
  )
}


#' exposure_input Server Functions
#'
#' @noRd 
mod_exposuretable_input_server <- function(id, modeldat, exposure_time_series){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    

    
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    
    
    # Render the exposure table ------------------------------------------------
    output[["exposure_table"]] <- rhandsontable::renderRHandsontable({
      #n_decimals <- min(5,max(count_decimal_places(default_exposure$conc)))
      rhandsontable::rhandsontable(
        cvasiUI::default_exposure,
        height=600
      ) %>%
        rhandsontable::hot_col("conc",
                               format = "0.00[000]"
                               )
    })
    
    # Reactive expression for the table content --------------------------------
    exposure_table <- reactive({

      rhandsontable::hot_to_r(input[["exposure_table"]])
      })

    # Plot of the tabulated values ---------------------------------------------
    output[["exposure_plot"]] <- renderPlot({
      req(length(exposure_table()) > 0)
      
      ggplot2::ggplot(exposure_table()) + 
        ggplot2::geom_area(ggplot2::aes(time,conc), 
                           alpha = 0.75,
                           position = "identity") + 
        ggplot2::facet_wrap(trial ~ ., ncol = 2)
      
    })
    
    # Check change of values and show in GUI ----------------------------------- 
    exp_vals_differ <- reactive({
      req(exposure_table())
      if (all(dim(exposure_time_series()) == dim(exposure_table()))){
        !all(exposure_time_series() == exposure_table())  
      }else{
        TRUE
      }
      
    })
    
    observeEvent(exp_vals_differ(), {
      shinyjs::toggleCssClass(id = "assign", 
                              class = "input-change", 
                              condition = exp_vals_differ())
    })
    
    # Assign values button observer --------------------------------------------
    observeEvent(input[["assign"]], {
      val <- exposure_table()
      exposure_time_series(val)
    })
    
  })
}
    
## To be copied in the UI
# mod_exposuretable_input_ui("exposure_input_1")
    
## To be copied in the server
# mod_exposuretable_input_server("exposure_input_1")
