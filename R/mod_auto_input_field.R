#' auto_input_field UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_auto_input_field_ui <- function(id){
  ns <- NS(id)
  tagList(
    #actionLink(ns("debug"), "debug"),
    tags$div(
      uiOutput(ns("inputField")),
      textOutput(ns("range_error")) %>% 
        tagAppendAttributes(class = "text-warning", 
                            style = "margin-top: -1em; margin-bottom: 0.5em")
    )
  )
}
    
#' auto_input_field Server Functions
#'
#' @noRd 
mod_auto_input_field_server <- function(id,
                                        label,
                                        value,
                                        min_value = NA,
                                        max_value = NA,
                                        datatype = NA,
                                        selected = NA,
                                        choices = NA,
                                        info_button = FALSE,
                                        info_tooltip = FALSE,
                                        info_txt = NA
                                        ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    label_ <- label # store the original label text without info button or tooltip
    # Add info to label ----
    ## Add info as button ----
    info_button_id <- paste0("info_",id)
    if (info_button & !is.na(info_txt)){
      label <- add_info_icon(label,
                             button_id = ns(info_button_id))
    }
    ## Add info as tooltip ----
    if (info_tooltip & !is.na(info_txt)){
      label <- tooltip_text(mytext = label, 
                                  tooltip = info_txt)
    }
    
    # Input field depending on the datatype ----
    output[["inputField"]] <- renderUI({
      tags$div(
        tagList(
          deactivateMouseWheel(),
          
          switch(datatype,
                 
                 logical = checkboxInput(
                   inputId = ns(id),
                   label = label,
                   value = value
                 ),
                 
                 numerical = numericInput(
                   inputId = ns(id),
                   label = label,
                   value = value
                 ) %>% set_lang(),
                 
                 text = textInput(
                   inputId = ns(id),
                   label = label,
                   value = value
                 ),
                 
                 dropdown = selectInput(
                   inputId = ns(id),
                   label = label,
                   selected = value,
                   choices = choices
                 ),
                 
                 textInput(
                   inputId = ns(id),
                   label = label,
                   value = value
                 )
          )
        )
        )# end of div
    }) # end of renderUI
    
    # reactive to track numeric value in range ----
    outside_range <- reactiveVal(TRUE)
    output[["range_error"]] <- renderText({
      req(input[[id]])
      error_text <- paste0("Outside range [",
                                     min_value,
                                     ",",
                                     max_value,
                                     "].")

      if (datatype == "numerical"){
        is_in_range <- in_range(input[[id]], 
                                min_value, 
                                max_value)
        if (!is_in_range){
          return(error_text)
        } else {
          return(NULL)
        }
        
      } else {
        return(NULL)
      }
    })
    
    # Observe info button for modal help ----
    observeEvent(input[[info_button_id]],{
      ns <- session$ns
      
      showModal(modalDialog(title = NULL,
                                  HTML(
                                    paste0(
                                      tags$h3(label_),
                                      "<br>",
                                      info_txt
                                    )
                                  ),
                                  footer=tagList(NULL),
                                  easyClose=TRUE)
      )
    }, ignoreInit = TRUE)
    
    
    # Return value ----
    return(reactive(input[[id]]))
    
    
  })
}
    
## To be copied in the UI
# mod_auto_input_field_ui("auto_input_field_1")
    
## To be copied in the server
# mod_auto_input_field_server("auto_input_field_1")
