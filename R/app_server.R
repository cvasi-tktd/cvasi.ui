#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import neofm
#' @importFrom magrittr `%>%`
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  mod_prediction_workflow_server("prediction_workflow")
  
}
