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
mod_epx_mtw_settings_server <- function(id, exposure_time_series, model_name = NA){
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
    
    # vary window length default based on model group ----
    # 3 for algae models, 7 for Lemna, 14 for Myriophyllum
    # use reactiveVal here to observer only the change of the value and not of
    # the reactive expression
    winlength_group <- reactiveVal() 
    observeEvent(model_name(), {
      dplyr::case_when(model_name() %>% grepl("^Algae_",.) ~ "Algae",
                       model_name() %>% grepl("^Lemna_",.) ~ "Lemna",
                       model_name() %>% grepl("^Myrio",.) ~ "Myrio",
                       .default = NA) %>% 
        winlength_group()
    })
    observeEvent(winlength_group(), {
      winlength_new <- dplyr::case_when(winlength_group() == "Algae" ~ 3,
                                        winlength_group() == "Lemna" ~ 7,
                                        winlength_group() == "Myrio" ~ 14,
                                        .default = 7)
      updateNumericInput(session = session, 
                         inputId = "window_length",
                         value = winlength_new
      )
    })
    
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
