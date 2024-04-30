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
    #actionButton(ns("debug"),"debug"),
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
      req(length(rvtl(forcings_time_series)))
      forcings_ts <- rvtl(forcings_time_series)[[selected_model() 
                                                 %>% class() %>% 
                                                   lookup_name()]]
      if (length(forcings_ts) > 0){
        if (all(req_f() %in% names(forcings_ts))){
          local_forcings_ts(forcings_ts)
          forcings_ts
        }else {
          o <- cvasiUI::model_defaults[[selected_model()%>% get_model_name()]][["forcing_defaults"]]
          local_forcings_ts(o)
          o
        }
      } else {
        o <- cvasiUI::model_defaults[[selected_model()%>% get_model_name()]][["forcing_defaults"]]
        local_forcings_ts(o)
        o
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
    ### Generate input fields --------------------------------------------------
    output[["f_constant_input"]] <- renderUI({
      model_name()
      print("model name changed")
      req_forcings <- isolate(req_f())
      req( length(req_forcings) > 0 )
      tagList(
        actionButton(ns("set_const_forcings"), "Assign values"),
        do.call(tagList,
                lapply(req_forcings, function(f_name) {
                  f_id <- ns(f_name)
                  numericInput(inputId = f_id,
                               label = f_name,
                               value = init_forcings()[[f_name]][1,"value"]
                  ) %>% set_lang()
                }
                )
        )
      )
    })
    
    ### Check change of values and show in GUI ---------------------------------
    const_f <- reactive({
      req( req_f() )
      req( length(req_f()) > 0 )
      do.call(req, lapply(req_f(), function(x) input[[x]]))
      input_f_vals <- lapply(setNames(req_f(), req_f()), function(f_name) {
        out <- data.frame(0, input[[f_name]])
        colnames(out) <- c("t", "value")
        out
      })
      return(input_f_vals)
    })
    
    const_f_diff <- reactive({
      forcings_ts <- rvtl(forcings_time_series)[[selected_model() 
                                                 %>% class() %>% 
                                                   lookup_name()]]
      req(const_f(), forcings_ts)
      if (length(forcings_ts)){
        md5_stored <- digest::digest(forcings_ts, algo = "md5")
        md5_changed <- digest::digest(const_f(), algo = "md5")
        return(md5_stored != md5_changed)
      } else {
        return(FALSE)
      }
    })
    
    observeEvent(list(input[["f_source"]], const_f_diff()), {
      shinyjs::toggleCssClass(id = "set_const_forcings",
                              class = "input-change",
                              condition = const_f_diff())
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    ### Set constant values --------------------------------------
    observeEvent(input[["set_const_forcings"]], {
      req( length(req_f()) > 0 )

      input_f_vals <- lapply(setNames(req_f(), req_f()), function(f_name) {
        out <- data.frame(0, input[[f_name]])
        colnames(out) <- c("t", "value")
        out
      })
      local_forcings_ts(input_f_vals)
      forcings_time_series[[selected_model() 
                            %>% class() %>% 
                              lookup_name()]] <- input_f_vals
    }, ignoreInit = TRUE)
    

    ## Variable forcings input -------------------------------------------------
    output[["f_variable_input"]] <- renderUI({
      fluidRow(
        column(4,
               tagList(
                 #fileInput(ns("forcings_inputfile"), label = "Upload forcings"),
                 fileInputOnlyButton(
                   ns("forcings_inputfile"),
                   buttonLabel=list(
                     icon("upload", class = NULL, lib = "font-awesome"),
                     "Upload forcings"
                   ),
                   accept=c(".txt",".csv"),
                   multiple = FALSE,
                   width=72
                 ),
                 textOutput(ns("error_text")),
                 actionButton(ns("set_var_forcings"), "Assign values")
               )
        ),
        column(8,
               tagList(
                 "The file should have three columns with the header 'time', 
                 'forcing' and 'value'. The columns should be semicolon-separated." %>% 
                   div(class="well") %>% 
                   as.character() %>% 
                   HTML(),
                plotOutput(ns("forcings_plot"))
               )
               )
      )
    })
    
    ### Load from file -----------------------------------
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
    
    ### Plot of loaded forcings timeseries -----------------------------------
    output[["forcings_plot"]] <- renderPlot({
      req(length(local_forcings_ts()) > 0)
      
      plot_dat <- list_rbind2(local_forcings_ts(), listnames_to = "forcing")
      
      ggplot2::ggplot(plot_dat) + 
        ggplot2::geom_area(ggplot2::aes(time,value), 
                           alpha = 0.75,
                           position = "identity") + 
        ggplot2::facet_wrap(forcing ~ ., ncol = 2, scales = "free")+ 
        ggplot2::theme(axis.text = ggplot2::element_text(size = 13),
                       axis.title = ggplot2::element_text(size = 14),
                       strip.text = ggplot2::element_text(size = 14)
        )
    })
    
    ### Check change of values and show in GUI ---------------------------------
    var_f_diff <- reactive({
      forcings_ts <- rvtl(forcings_time_series)[[selected_model() 
                                                 %>% class() %>% 
                                                   lookup_name()]]
      if (length(local_forcings_ts())){
        md5_stored <- digest::digest(forcings_ts, algo = "md5")
        md5_changed <- digest::digest(local_forcings_ts(), algo = "md5")
        return(md5_stored != md5_changed)
      } else {
        return(FALSE)
      }
    })
    
    observeEvent(list(var_f_diff(), input[["set_var_forcings"]]), {
      shinyjs::toggleCssClass(id = "set_var_forcings",
                              class = "input-change",
                              condition = var_f_diff())
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    ### Set time-variable forcings ----------------------------
    observeEvent(input[["set_var_forcings"]], {
      req( length(local_forcings_ts()) > 0 )
      forcings_time_series[[selected_model() 
                            %>% class() %>% 
                              lookup_name()]] <- local_forcings_ts()
    })



    
  })
}
    
## To be copied in the UI
# mod_forcings_input_ui("forcings_input_1")
    
## To be copied in the server
# mod_forcings_input_server("forcings_input_1")
