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
#' @global time conc
#' @noRd 
mod_exposuretable_input_server <- function(id, modeldat, exposure_time_series){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    # constants ----------------------------------------------------------------
    units_ <- c(time = "d",conc = "mg/l", trial = "-")
    
    # Render the exposure table ------------------------------------------------
    output[["exposure_table"]] <- rhandsontable::renderRHandsontable({
      old_colnames <- cvasi.ui::default_exposure %>% 
        colnames()
      new_colnames <- cvasi.ui::default_exposure %>% 
        colnames() %>% 
        paste0(" [",units_,"]")
      
      n_decimals <- 7
      rhandsontable::rhandsontable(
        cvasi.ui::default_exposure,
        height=600,
        colHeaders = new_colnames
      ) %>%
        rhandsontable::hot_col(new_colnames[old_colnames == "conc"],
                               renderer =
                               'function(instance, td, row, col, prop, value, cellProperties) {
                                Handsontable.renderers.NumericRenderer.apply(this, arguments);
                                var locale = d3.formatLocale({
                                  decimal: ".",
                                  thousands: "",
                                  grouping: [3]
                                });
                                var fformat = locale.format(",");
                                var roundedValue = parseFloat(value).toFixed(7); // Limit to 7 decimal places
                                td.innerHTML = fformat(roundedValue);
                            }'
        )
    })
    
    # Reactive expression for the table content --------------------------------
    exposure_table <- reactive({
      rhandsontable::hot_to_r(input[["exposure_table"]])
      })

    # Plot of the tabulated values ---------------------------------------------
    output[["exposure_plot"]] <- renderPlot({
      req(length(exposure_table()) > 0)
      
      unit_time <- units_[["time"]]
      unit_conc <- units_[["conc"]]
      
      ggplot2::ggplot(exposure_table()) +
        ggplot2::geom_area(ggplot2::aes(time,conc),
                           alpha = 0.75,
                           position = "identity") +
        ggplot2::xlab(paste0("Time [",unit_time,"]")) + 
        ggplot2::ylab(paste0("Concentration [",unit_conc,"]")) +
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
