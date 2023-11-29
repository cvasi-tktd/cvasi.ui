#' forcings_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_forcings_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h2("Forcings"),
    radioButtons(ns("f_source"), "Choose ", 
                 choices = list(
                   `constant forcing` = "constant" ,
                   `time-variable forcing` = "variable"
                 )
    ),
    uiOutput(ns("f_choice"))
  )
}
    
#' forcings_input Server Functions
#'
#' @noRd 
mod_forcings_input_server <- function(id, selected_model, forcings_time_series){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input[["debug"]], browser())
    
    # Local reactive values ----------------------------------------------------
    local_forcings_ts <- reactiveVal()
    
    
    # Reactives ----------------------------------------------------------------
    model_name <- reactive(get_model_name(selected_model()))
    
    req_f <- reactive(
      get_required(selected_model(), "forcings")
      )

    init_forcings <- reactive({
      if (length(forcings_time_series()) > 0){
        if (all(req_f() %in% names(forcings_time_series()))){
          forcings_time_series()
        }else {
          forcing_defaults
        }
      } else {
        forcing_defaults
      }
    })
    
    

    # Forcings input -----------------------------------------------------------
    
    output[["f_choice"]] <- renderUI({
      if (input[["f_source"]] == "constant"){
        uiOutput(ns("f_constant_input"))
      } else if (input[["f_source"]] == "variable"){
        uiOutput(ns("f_variable_input"))
      } else {
        NULL
      }
    })
    
    ## Constant forcings input -------------------------------------------------
    output[["f_constant_input"]] <- renderUI({
      model_name()
      print("model name changed")
      req_forcings <- isolate(req_f())
      req( length(req_forcings) > 0 )
      tagList(
        actionButton(ns("set_const_forcings"), "set forcings"),
        do.call(tagList,
                lapply(req_forcings, function(f_name) {
                  #print(paste0(f_name,": ",init_forcings()[[f_name]][,f_name]))
                  f_id <- ns(f_name)
                  numericInput(inputId = f_id,
                               label = f_name,
                               value = init_forcings()[[f_name]][,f_name]
                  )
                }
                )
        )
      )
    })
    
    observeEvent(input[["set_const_forcings"]], {
      req( length(req_f()) > 0 )
      
      input_f_vals <- lapply(setNames(req_f(), req_f()), function(f_name) {
        out <- data.frame(0, input[[f_name]])
        colnames(out) <- c("t", f_name)
        out
      })
      forcings_time_series(input_f_vals)
    })
    
    ## Variable forcings input -------------------------------------------------
    output[["f_variable_input"]] <- renderUI({
      fluidRow(
        column(4,
               tagList(
                 fileInput(ns("forcings_inputfile"), label = "Upload forcings"),
                 textOutput(ns("error_text")),
                 actionButton(ns("set_var_forcings"), "set forcings")
               )
        ),
        column(8,
               plotOutput(ns("forcings_plot"))
               )
      )
    })
    
    
    observeEvent(input[["forcings_inputfile"]], {
      tryCatch({
        shinyjs::html("error_text", "")
        
        filepath <- input[["forcings_inputfile"]]$datapath
        filename <- input[["forcings_inputfile"]]$name
        message(paste0("load file from ",filepath, " with original filename: ", filename))
        
        out <- import_forcings(filepath)
        check_forcings_ts(forcings = out, expected_forcings = req_f())
        
        
        local_forcings_ts(out)
        
      }, error = function(cond) {
        shinyjs::html(id = "error_text", 
                      html = paste0("<div class = \"alert alert-danger\">",as.character(cond),"</div>"),
                      add = TRUE)
      })
      
    })

    output[["forcings_plot"]] <- renderPlot({
      req(length(local_forcings_ts()) > 0)
      
      plot_dat <- list_rbind2(local_forcings_ts(), listnames_to = "forcing")
      
      ggplot2::ggplot(plot_dat) + 
        ggplot2::geom_area(ggplot2::aes(time,value), alpha = 0.75) + 
        ggplot2::facet_wrap(forcing ~ ., ncol = 2, scales = "free")
    })
    
    observeEvent(input[["set_var_forcings"]], {
      req( length(local_forcings_ts()) > 0 )
      forcings_time_series(local_forcings_ts())
    })



    
  })
}
    
## To be copied in the UI
# mod_forcings_input_ui("forcings_input_1")
    
## To be copied in the server
# mod_forcings_input_server("forcings_input_1")
