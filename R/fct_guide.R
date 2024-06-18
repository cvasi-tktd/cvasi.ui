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
      description = "...to the guided tour of ***cvasi*** the R Shiny web app for
      '**C**alibration, **VA**lidation, **SI**mulation and endpoint calculation of TKTD models'.  
      \\
      To navigate through the tour, use the displayed buttons or the arrow keys of your keyboard.
      To exit the tour hit the [esc] key, the 'Close' or 'Done' button.",
      position = "bottom-center"
    ),
    list(
      el = "prediction_workflow-active_model_wrapper",
      title = "Choose model",
      description = "Choose one of the provided models.  
      There are a couple of primary producer models available but for this tutorial
      choose the 'Lemna_Schmitt' model. In the adjacent textbox, you will 
      find further information for this and the other models.",
      position = "bottom"
    ),
    list(
      el = "prediction_workflow-parameters_wrapper",
      title = "Set parameters",
      description = "Here you can set 
      values to the parameters of the chosen model. These parameters are 
      separated into several thematic groups that guide you on the parameter's 
      function. Within each of these groups, 'expert' parameters (with preset values)
      are hidden to allow focus on the parameters that are most commonly modified. 
      The 'expert' parameters can be made visible and edited.  
      You can retrieve further brief information for each parameter if you hover 
      your mouse pointer over the parameter name.  
      To accept changes use the 'Assign values' button.",
      position = "top"
    ),  
    list(
      el = "prediction_workflow-init_wrapper",
      title = "Set init values",
      description = "Set initial values for state variables such as biomass
      at the beginning of a simulation.  
      Set values by clicking on the 'Assign values' button. Find out more 
      about this initial value by hovering your mouse pointer over the title or 
      click on the **i**nformation icon."
    ),
    list(
      el = "prediction_workflow-forcings_wrapper",
      title = "Forcings",
      description = "Forcings are environmental factors (other than the 'exposure')
      that affect the life of the simulated individuals. Depending on the model, 
      different forcings are required. For instance, the 'Lemna_Schmitt' model 
      requires two forcings, the temperature 'temp' and the wavelength of the 
      electromagnetic radiation (i.e. light) 'rad', whereas the Myriophyllum models 
      are defined without any forcings.  
      For this tutorial select the constant forcings and hit 'Assign values'; then continue."
    ),
    list(
      el = "prediction_workflow-exposure_box_title",
      title = "Exposure",
      description = "Exposure is another crucial component that describes the timing 
      and concentration to which the simulated organisms are exposed to a toxic substance. 
      Here, you can set the exposure profile in different ways."
    ),
    list(
      el = "prediction_workflow-check_wrapper",
      title = "Check fields",
      description = "In the previous steps, you have hopefully entered all necessary
      information.  
      The 'input check' fields give you hints about where data is missing 
      before you execute the calculations."
    ),  
    list(
      el = "prediction_workflow-exposure_input-exposure_source",
      title = "Exposure data sources",
      description = "To provide the source of an exposure profile:  
      - Set the values directly in the **editable table**.  
      - Upload a custom **file** with tab-separated columns named 'time' and 'conc'.  
      - Upload a **FOCUS TOXSWA out-file** (TOXSWA versions 4 and 5.5.3 are supported) 
      from which the exposure profile of the parent compound is extracted.  
      In this tutorial use the editable table."
    ),  
    list(
      el = "prediction_workflow-output_box_title",
      title = "Model output",
      description = "When all the model inputs are correctly set, you are ready to 
      generate the model output."
    ),
    list(
      el = "prediction_workflow-prediction-epx_fields_wrapper",
      title = "epx settings",
      description = "However, first, define the **settings for the EPx 
      moving time window** procedure. This procedure does the effect calculations
      within the set window length while the window moves through the exposure 
      time series in steps set by 'window interval'.  
      Therefore, if you have used an exposure profile which has values for 21 days, 
      you should set the 'window length' to a value lower than 21 to get more 
      than one window and hence meaningful results."
    ),
    list(
      el = "prediction_workflow-prediction-predict",
      title = "Predict",
      description = "As a final step click the 'Predict' button to generate the 
      model output."
    )
  )
}


