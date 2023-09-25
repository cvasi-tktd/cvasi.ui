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
  
  GUTS_RED_IT() %>%
    set_param(c(kd=0.0005,hb=0,alpha=0.4,beta=1.5)) %>%
    set_exposure(data.frame(t=c(0,100,101,200,201,400),pec=c(0,0,0.1,0.1,0,0))) -> scenario
  
  output[["showscen"]] <- renderPrint(scenario)
  
}
