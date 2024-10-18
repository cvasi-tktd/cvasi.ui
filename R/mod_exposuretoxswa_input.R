#' exposurefile_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_exposuretoxswa_input_ui <- function(id){
  ns <- NS(id)
  tagList(

   #actionButton(ns("debug"),"debug"),
    fileInputOnlyButton(
      ns("importExpProfiles"),
      buttonLabel=list(
        icon("upload", class = NULL, lib = "font-awesome"),
        "Upload exposure profiles"
      ),
      accept=c(".out"),
      multiple = FALSE,
      width=72
    ),
    textOutput(ns("import_error")),
    uiOutput(ns("substance_selection")),
    actionButton(ns("assign"), "Assign values"),

    plotOutput(ns("exposure_plot"))
  )
}
    
#' exposurefile_input Server Functions
#'
#' @global time conc
#' @noRd 
mod_exposuretoxswa_input_server <- function(id, exposure_time_series){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input[["debug"]], browser())
    
    # Reactive values ----------------------------------------------------------
    file_content <- reactiveVal()

    # Reactives ----------------------------------------------------------------
    substance_choices <- reactive({
      req(length(file_content()) > 0)
      names(file_content())
    })
    
    toxswa_units <- reactive({
      req(length(input[["sub_name"]]))
      
      sub_name <- input[["sub_name"]]
      out <- file_content()[[sub_name]] %>% 
        attributes() %>% 
        .$units
      if( is.null(out)){
        out <- c(time = NA, conc = NA)
      }
      return(out)
    })
    
    
    # File import --------------------------------------------------------------
    observeEvent(input[["importExpProfiles"]], {
      shinyjs::html("import_error", html = "")
      filesToImport <- input[["importExpProfiles"]]$datapath
      actualFileNames <-  input[["importExpProfiles"]]$datapath %>% basename() %>% tools::file_path_sans_ext()
      fileNames <- input[["importExpProfiles"]]$name %>% tools::file_path_sans_ext()
      if (length(filesToImport)) {
        if(length(filesToImport) > 1)
          stop("importing more than one file not supported yet")
        shinybusy::show_modal_spinner(text = "loading Toxswa profile")
        
        expProfilesImported <- cvasi::import_toxswa(filesToImport, split=TRUE)
        # limit series to the two important columns, i.e. time (1) and conc (3)
        for(nm in names(expProfilesImported)) {
          expProfilesImported[[nm]] <- expProfilesImported[[nm]] %>% 
            dplyr::select(time=1, conc=3) %>%
            dplyr::mutate(trial=fileNames[1]) %>%
            units::drop_units()
        }
        # rename list entries to reflect original filename
        newNames <- gsub(paste0("^", actualFileNames[1], "_"), paste0(fileNames[1], "_"), names(expProfilesImported))
        expProfilesImported <- setNames(expProfilesImported, newNames)
        
        shinybusy::remove_modal_spinner()
        if(length(expProfilesImported)){
          file_content(expProfilesImported)
        } else {
          shinyjs::html("import_error", 
                        html = paste0("<div class = \"text-warning\">No file loaded.</div>"))
        }
          
        
      }
    })
    
    # substance selection ----
    output[["substance_selection"]] <- renderUI({
      req(length(substance_choices) > 0)
      tagList(
        shiny::selectInput(ns("sub_name"), "Select substance", 
                         choices = substance_choices())
      )
    })
    
    # Plot of the imported file contents --------------------------------------
    output[["exposure_plot"]] <- renderPlot({
      req(length(file_content()) > 0)
      req(input[["sub_name"]])
      sub_name <- input[["sub_name"]]
      
      unit_conc <- toxswa_units()[["conc"]]
      unit_time <- toxswa_units()[["time"]]
      
      ggplot2::ggplot(file_content()[[sub_name]]) +
        ggplot2::geom_area(ggplot2::aes(time,conc),
                           alpha = 0.75,
                           position = "identity") +
        ggplot2::xlab(paste0("Time [",unit_time,"]")) + 
        ggplot2::ylab(paste0("Concentration [",unit_conc,"]")) +
        ggplot2::facet_wrap(trial ~ ., ncol = 2)
    })
    
    # Assign values button observer --------------------------------------------
    observeEvent(input[["assign"]], {
      sub_name <- input[["sub_name"]]
      val <- file_content()[[sub_name]]
      exposure_time_series(val)
    })
    
    # Check change of values and show in GUI ----------------------------------- 
    exp_vals_differ <- reactive({
      req(file_content())
      req(input[["sub_name"]])
      sub_name <- input[["sub_name"]]
      if (all(dim(exposure_time_series()) == dim(file_content()[[sub_name]]))){
        !all(exposure_time_series() == file_content()[[sub_name]])  
      }else{
        TRUE
      }
      
    })
    
    observeEvent(exp_vals_differ(), {
      shinyjs::toggleCssClass(id = "assign", 
                              class = "input-change", 
                              condition = exp_vals_differ())
    })
    
  })
}
    
## To be copied in the UI
# mod_exposurefile_input_ui("exposurefile_input_1")
    
## To be copied in the server
# mod_exposurefile_input_server("exposurefile_input_1")
