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
    actionButton(ns("assign"), "Assign values"),

    plotOutput(ns("exposure_plot"))
  )
}
    
#' exposurefile_input Server Functions
#'
#' @noRd 
mod_exposuretoxswa_input_server <- function(id, exposure_time_series){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input[["debug"]], browser())
    
    # Reactive values ----------------------------------------------------------
    file_content <- reactiveVal()

    # File import --------------------------------------------------------------
    observeEvent(input[["importExpProfiles"]], {
      filesToImport <- input[["importExpProfiles"]]$datapath
      fileNames <- input[["importExpProfiles"]]$name %>% tools::file_path_sans_ext()
      if (length(filesToImport)){
        shinybusy::show_modal_spinner(text = "loading Toxswa profile")
        expProfilesImported <- readToxswaExpProfiles(filesToImport, fileNames)
        shinybusy::remove_modal_spinner()
        if(length(expProfilesImported)){
          file_content(expProfilesImported[[1]])
        } else {
          shinyjs::html("import_error", 
                        html = paste0("<div class = \"text-warning\">No file loaded.</div>"))
        }
          
        
      }
    })
    
    ## Plot of the imported file contents --------------------------------------
    output[["exposure_plot"]] <- renderPlot({
      req(length(file_content()) > 0)
      
      ggplot2::ggplot(file_content()) + 
        ggplot2::geom_area(ggplot2::aes(time,conc),
                           alpha = 0.75,
                           position = "identity") + 
        ggplot2::facet_wrap(trial ~ ., ncol = 2)
    })
    
    # Assign values button observer --------------------------------------------
    observeEvent(input[["assign"]], {
      val <- file_content()
      exposure_time_series(val)
    })
    
    # Check change of values and show in GUI ----------------------------------- 
    exp_vals_differ <- reactive({
      req(file_content())
      if (all(dim(exposure_time_series()) == dim(file_content()))){
        !all(exposure_time_series() == file_content())  
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
