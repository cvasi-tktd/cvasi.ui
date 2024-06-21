#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import cvasi
#' @importFrom magrittr `%>%`
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  options(shiny.maxRequestSize = 500*1024^2) 
  Sys.setlocale("LC_ALL", "en_US.UTF-8")
  
  mod_prediction_workflow_server("prediction_workflow")
  
  observeEvent(input[["guide"]],{
    #shinyjs::runjs("$('[data-widget=\"collapse\"]').click();") # expand all boxes
    #shinyjs::runjs("$('.box.collapsed-box').removeClass('collapsed-box');")
    shinyjs::runjs("$('[data-widget=\"collapse\"]').each(function() {
      if ($(this).find('.fas.fa-plus').length > 0) {
        $(this).click();
      }
    });") # toggle the boxes only if the boxes are collapsed => expand all boxes
    
    guide(cicerone_textblocks())$init()$start()
  })
  
  observeEvent(input[["expand_boxes"]],{
    #shinyjs::runjs("$('[data-widget=\"collapse\"]').click();")
    shinyjs::runjs("$('[data-widget=\"collapse\"]').each(function() {
      if ($(this).find('.fas.fa-plus').length > 0) {
        $(this).click();
      }
    });") # toggle the boxes only if the boxes are collapsed => expand all boxes
  })
  
  observeEvent(input[["collapse_boxes"]],{
    #shinyjs::runjs("$('[data-widget=\"collapse\"]').click();")
    shinyjs::runjs("$('[data-widget=\"collapse\"]').each(function() {
      if ($(this).find('.fas.fa-minus').length > 0) {
        $(this).click();
      }
    });") # toggle the boxes only if the boxes are collapsed => collapse all boxes
  })

  
}
