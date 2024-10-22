#' exposure_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_exposure_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        radioButtons(inputId = ns("exposure_source"), 
                     label = "Source",
                     choices = list(Table = "table",
                                    `Custom file` = "custom",
                                    `TOXSWA file` = "toxswa"
                     )
        ),
        width = 3
      ),
      column(
        textOutput(ns("input_description")),
        downloadLink(outputId = ns("download_expo_template"), label = "Download template"),
        width = 9 
      ) 
    ),# end of fluidRow
    fluidRow(
      uiOutput(ns("input_mod"))
    )
  )
}


#' exposure_input Server Functions
#'
#' @noRd 
mod_exposure_input_server <- function(id, modeldat, exposure_time_series){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    
    # input modules for the chosen input source --------------------------------
    output[["input_mod"]] <- renderUI({
      if (input[["exposure_source"]] == "table"){
        mod_exposuretable_input_ui(ns("exposuretable_input"))
      }else if (input[["exposure_source"]] == "custom"){
        mod_exposurefile_input_ui(ns("exposurefile_input"))
      } else if (input[["exposure_source"]] == "toxswa"){
        mod_exposuretoxswa_input_ui(ns("exposuretoxswa_input"))
      } else {
        NULL
      }
    })
    
    
    # explanation text for the chosen input source ------------------------------
    shinyjs::hide("download_expo_template")
    observeEvent(input[["exposure_source"]],{
      if (input[["exposure_source"]] == "custom"){
        custom_file_help_text <- "The file should have at least two columns with
        the header 'time' and 'conc'. An optional third column with header 'trial'
        could be added, if several trials are used. The columns should be tab-separated." %>% 
      div(class = "smaller_italic") %>% 
      as.character()
      } else {
        custom_file_help_text <- ""
      }
      
      shinyjs::html(
        "input_description",
        html = custom_file_help_text
      )
      
      shinyjs::toggle("download_expo_template", 
                      condition = input[["exposure_source"]] == "custom")
      
    })
  
  output[["download_expo_template"]] <- downloadHandler(
    filename = "exposure_template.txt",
    content = function(file) {
      message(file)
      # Write the dataset to the `file` that will be downloaded
      template_exposure(filepath = file, example_data = TRUE)
    }
  )
  
  # Module servers -----------------------------------------------------------
  ## Exposure table input module server --------------------------------------
  mod_exposuretable_input_server("exposuretable_input", modeldat, exposure_time_series)
  
  ## Exposure file input module server ---------------------------------------
  mod_exposurefile_input_server("exposurefile_input", exposure_time_series)
  
  ## Exposure toxswa input module server -------------------------------------
  mod_exposuretoxswa_input_server("exposuretoxswa_input", exposure_time_series)
  
  })
}

# Read an exposure profile
#
# Read a single exposure profile at 'pathtofile' from a character seperated
# text file in a table format. The expected table format includes the columns
# `time` and `conc` for the concentration at each timestep.
#
# @param pathtofile the path where the exposure profile is found
# @param sep the field separator character
#
# @return the exposure profile
#' @importFrom methods is
read_single_exposure <- function(pathtofile, sep = "\t"){
  profile <- utils::read.table(pathtofile, header = TRUE, sep = sep) %>%
    attempt::attempt(msg=paste0("Could not read file ",pathtofile))
  
  if (is(profile, "try-error")) {
    profile <- NULL
  }
  
  checkedProfile <- check_profile(profile)
  
  if(length(checkedProfile) & !("trial" %in% colnames(checkedProfile))){
    checkedProfile[,"trial"] <- "1"
  } else {
    checkedProfile[,"trial"] <- as.character(checkedProfile[,"trial"])
  }
  
  return(checkedProfile)
}


# Import exposure profiles from text files
#
# Read several exposure profiles at their corresponding paths at 'pathtofiles'.
# The exposure profiles are read for each file that are character seperated
# text files in a table format. The expected table format includes the columns
# `time` and `conc` for the concentration at each timestep.
#
# @param pathtofiles path to files
# @param profileNames the names of the profiles
# @param sep the field separator character
#
# @return list of exposure profiles
import_exposure_text <- function(pathtofiles, profileNames=NA, sep = "\t"){
  if ( any(sapply(profileNames, is.na)) ) {
    profileNames <- basename(pathtofiles) %>%
      tools::file_path_sans_ext()
  } else {
    profileNames <- profileNames
  }
  raw_profiles <- lapply(pathtofiles, read_single_exposure, sep = sep)
  names(raw_profiles) <- profileNames
  
  # any incorrect profiles?
  flagCorrect <- do.call(c, lapply(raw_profiles,function(x) length(x)!=0))
  raw_profiles <- raw_profiles[flagCorrect]
  
  
  return(raw_profiles)
}

# Check exposure profile
#
# @param p the exposure profile
#
# @return the profile or NULL if not correct
check_profile <- function(p){
  # check if has content
  if (!length(p)){
    out <- "File is empty"
    warning(out)
    return(NULL)
  }
  
  
  
  # check if two or more columns
  if (ncol(p) < 2){
    out <- "File is not a plain-text file with two or more columns."
    warning(out)
    return(NULL)
  }
  
  # check if necessary columns are available
  if (!all( c("time", "conc") %in% colnames(p) )){
    out <- "Not all necessary columns ('time' and 'conc') available."
    warning(out)
    return(NULL)
  }
  
  # check if columns give numeric data
  colClasses <- c(class(p[,'time']), class(p[,'conc']))
  acceptedClass <- !any(!(colClasses %in% c("numeric", "integer")))
  if (!acceptedClass){
    out <- "The columns are neither 'numeric' nor 'integers'."
    warning(out)
    return(NULL)
  }
  out <- p
  return(out)
  
}


## To be copied in the UI
# mod_exposure_input_ui("exposure_input_1")

## To be copied in the server
# mod_exposure_input_server("exposure_input_1")
