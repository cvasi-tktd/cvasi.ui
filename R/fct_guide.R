#' Cicerone guided tour 
#' 
#' This is a function to create the guided tour through the shiny app
#' explaining the main functions and giving hints
#'              
#' @param textblockslist a list specifiying the textblocks created by ```cicerone_textblocks()```
#' @param desc_md is the "description" element written in markdown and should be parsed to html?
#'
#' @return The Cicerone "Guided Tours" object
#' @seealso \code{\link{cicerone_textblocks}}
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
#' The text of the `description` element can be written in markdown.
#' Other named elements that are passed to `cicerone::Cicerone()$step()` are optional.
#'
#' @return A list of lists where each of the lists represents a step in the cicerone tour.
#' 
#' @seealso \code{\link{guide}}
#' @noRd
cicerone_textblocks <- function(){
  list(
    list(
      el = "guide",
      title = "Welcome",
      description = "...to the guided tour of *cvasi* the R Shiny web app for
      'Calibration, VAlidation, SImulation and endpoint calculation of TKTD models'.  
      \\
      To move through the tour, use the displayed buttons or the arrow keys of your keyboard.
      To exit the tour hit the [esc] key, the 'Close' or 'Done' button.",
      position = "bottom-center"
    ),
    list(
      el = "prediction_workflow-active_model_wrapper",
      title = "Choose model",
      description = "As a first step choose one of the provided models.  
      There are a couple of primary producer models available but for this tutorial
      choose the 'Lemna_Schmitt' model. In the adjacent textbox you will be able
      to find further information for this and the other models.",
      position = "bottom"
    ),
    list(
      el = "prediction_workflow-check_wrapper",
      title = "Check fields",
      description = "In the next steps you will be able to enter model inputs, 
      such as parameter values and exposure profiles/time series.  
      These 'input check' fields here will give you hints where data is missing 
      before you will be able to execute the calculations."
    ),
    list(
      el = "prediction_workflow-parameters_wrapper",
      title = "Set parameter",
      description = "The core of a model is its parameters. Here you can set 
      values to all of the parameters of the chosen model. These parameters are 
      separated into several thematic groups to guide you on the parameter's 
      function. Within each of these groups a set of 'expert' parameters is hidden 
      to further guide you to the parameters that most commonly are modified. 
      However, the 'expert' parameters can be made visible and edited if desired.  
      You can retrieve further brief information for each parameter if you hover 
      your mouse pointer over the parameter name.",
      position = "top"
    ),  
    list(
      el = "prediction_workflow-init_wrapper",
      title = "Set init values",
      description = "The initial values set the values of state variables like biomass
      at the beginning of a simulation.  
      In this tutorial make sure that the endpoint 'BM' is 1. Set the value by clicking
      on the 'Assign values' button. Find out more about this initial value by 
      moving your mouse pointer over the title and answer to yourself why this 
      initial value should be larger than 0."
    ),
    list(
      el = "prediction_workflow-forcings_wrapper",
      title = "Forcings",
      description = "Forcings are environmental factors (other than the 'exposure')
      that affect the life of the simulated individuals. Depending on the model 
      forcings are defined. The 'Lemna_Schmitt' model has two forcings, the 
      ambient temperature 'temp' and the wavelength of the electromagnetic radiation
      (i.e. light) 'rad', whereas the Myrophyllum models are defined without any forcings.  
      For this tutorial select the constant forcings and hit 'Assign values' then continue."
    ),
    list(
      el = "prediction_workflow-exposure_box_title",
      title = "Exposure",
      description = "Exposure is another crucial component that describes when 
      and in what quantity the simulated organisms are exposed to a toxic substance. 
      In this web application you can set the exposure profile in different 
      ways."
    ),  
    list(
      el = "prediction_workflow-exposure_input-exposure_source",
      title = "Exposure data sources",
      description = "You can provide an exposure profile via three different sources.  
      You can set values of an exposure profile directly in the **editable table**.  
      You can upload a **file** with tab-separated columns named 'time' and 'conc'.  
      And as the third option you can upload a **FOCUS TOXSWA out-file** 
      (TOXSWA versions 4 and 5.5.3 are supported) from which the exposure profile 
      of the parent is extracted.  
      In this tutorial use the editable table."
    ),  
    list(
      el = "prediction_workflow-output_box_title",
      title = "Model output",
      description = "When all the model inputs are correctly set, you can generate 
      the model output."
    ),
    list(
      el = "prediction_workflow-prediction-epx_fields_wrapper",
      title = "epx settings",
      description = "However, first you have to set the **settings for the EPx 
      moving time window** procedure.  
      If you have used an exposure profile which has values for 21 days, 
      you should set the 'window length' to a value lower than 21 to get meaningful 
      results."
    ),
    list(
      el = "prediction_workflow-prediction-predict",
      title = "Predict",
      description = "As a final step click the 'Predict' button to generate the 
      model output."
    )
  )
}


