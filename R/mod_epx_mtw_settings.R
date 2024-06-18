#' epx_mtw_settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_epx_mtw_settings_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_fields"))
  )
}

#' epx_mtw_settings Server Functions
#'
#' @noRd 
mod_epx_mtw_settings_server <- function(id, exposure_time_series){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input[["debug"]], browser())
    
    
    info_txt <- list(level = list(id = "level", 
                                  title = "% Effect level", 
                                  desc = "The level \"x\" at which the effect is evaluated.",
                                  value = 50),
                     factor_cutoff = list(id = "factor_cutoff", 
                                          title = "Cutoff value", 
                                          desc = "The value above which the EPx is not evaluated exactly.",
                                          value = 1000),
                     window_length = list(id = "window_length", 
                                          title = "Window length", 
                                          desc = "The length of each moving time window for which the EPx is calculated.",
                                          value = 7),
                     window_interval = list(id = "window_interval", 
                                            title = "Window interval", 
                                            desc = "The time interval a window moves each step.",
                                            value = 1)
    )
    
    # render input fields ----
    output[["input_fields"]] <- renderUI({
      tags$div(
        do.call(tagList, 
                lapply(info_txt, 
                       function(x){
                         tagList(
                           div(
                             numericInput(
                               inputId = ns(x$id),
                               label = tooltip_text(
                                 x$title,
                                 tooltip = x$desc
                               ), 
                               value = x$value),
                             if (x$id == "window_length"){
                               tags$div(id = ns("win_length_warn"), 
                                        class = "warnings",
                                        "")
                             }
                           )
                         )
                       })),
        class = "inputfields_flexbox"
      )
      
    })
    
    # Toggle warning if window length is longer than exposure time series ----
    observeEvent(input[["window_length"]], {
      if (max(exposure_time_series()$time) < input[["window_length"]]){
        shinyjs::html("win_length_warn", "Time series exceeded.")
      } else {
        shinyjs::html("win_length_warn", "")
      }
    })

    # return value ----
    epx_mtw_settings <- reactive({
      list(
        level = input[["level"]],
        factor_cutoff = input[["factor_cutoff"]],
        window_length = input[["window_length"]],
        window_interval = input[["window_interval"]]
      )
    })
    
    return(epx_mtw_settings)
    
  })
}

## To be copied in the UI
# mod_epx_mtw_settings_ui("epx_mtw_settings_1")

## To be copied in the server
# mod_epx_mtw_settings_server("epx_mtw_settings_1")
