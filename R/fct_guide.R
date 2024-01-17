#' Cicerone guided tour 
#' 
#' This is a function to create the guided tour through the shiny app
#' explaining the main functions and giving hints
#'              
#' @param textblockslist a list specifiying the textblocks created by ```cicerone_textblocks()```
#' @param desc_md is the "description" element written in markdown and should be parsed to html?
#'
#' @return The Cicerone "Guided Tours" object
#' @source https://cicerone.john-coene.com/
#'
#' @noRd
guide <- function(textblockslist, desc_md = TRUE){
  g <- cicerone::Cicerone$
    new(
      allow_close = FALSE
    )
  
  if (!missing(textblockslist)){
    if (desc_md){
      textblockslist <- lapply(textblockslist, function(x){
        x[["description"]] <- commonmark::markdown_html(x[["description"]])
        x
      } )
    }
    
    for (i in 1:length(textblockslist)){
      g <- do.call(g$step, textblockslist[[i]])
    }
  }
  
  g
}

#' Generate textblocks for a cicerone tour
#' 
#' This function generates a list of steps used for a cicerone tour. 
#' Use ```guide(cicerone_textblocks())``` to generate the cicerone object.
#' It is mandatory that each list has the named elements `el`, `title` and `description`.
#' Other named elements that are passed to `cicerone::Cicerone()$step()` are optional.
#'
#' @return A list of lists where each of the lists represents a step in the cicerone tour.
#' @noRd
cicerone_textblocks <- function(){
  list(
    list(
      el = "guide",
      title = "Welcome",
      description = "...to the guided tour of XYZ.\\
      To move through the tour use the buttons or the arrow keys of your keyboard.
      To exit the tour hit the [esc] key or the 'close' button.",
      position = "bottom-center"
    ),
    list(
      el = "prediction_workflow-active_model_wrapper",
      title = "Choose model",
      description = "As a first step choose one of the provided models.\\
      So far only primary producer models are available and for this tutorial choose the 'Lemna_schmitt' model.
      Further information for this model is provided in the adjacent textbox.",
      position = "right-bottom"
    ),
    list(
      el = "prediction_workflow-check_wrapper",
      title = "Check fields",
      description = "These fields will give the hint where data is missing before
    you can execute the calculations."
    ),
    list(
      el = "prediction_workflow-parameters_wrapper",
      title = "Set parameter",
      description = "klsdjflkdjasflkjsdfkljsaflkj."
    ),  
    list(
      el = "prediction_workflow-init_wrapper",
      title = "Set init values",
      description = "klsdjflkdjasflkjsdfkljsaflkj."
    ),
    list(
      el = "prediction_workflow-forcings_wrapper",
      title = "Forcings",
      description = "forcings box..."
    ),
    list(
      el = "prediction_workflow-exposure_box_title",
      title = "Exposure",
      description = "exposure box..."
    ),  
    list(
      el = "prediction_workflow-exposure_input-exposure_source",
      title = "Exposure data sources",
      description = "Select the source for the exposure data"
    ),  
    list(
      el = "prediction_workflow-output_box_title",
      title = "Model output",
      description = "Here the output will be generated."
    ),
    list(
      el = "prediction_workflow-prediction-epx_fields_wrapper",
      title = "epx settings",
      description = "set epx..."
    ),
    list(
      el = "prediction_workflow-prediction-predict",
      title = "Predict",
      description = "Predict button...."
    )
  )
}


