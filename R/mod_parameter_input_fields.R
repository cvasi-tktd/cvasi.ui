#' parameter_input_fields UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_fields_ui <- function(id){
  ns <- NS(id)
  tagList(
    #actionButton(ns("debug"), "debug"),
    uiOutput(ns("all_fields"))
    
  )
}
    
#' parameter_input_fields Server Functions
#'
#' @noRd 
mod_input_fields_server <- function(id, modeldat, type = "param"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input[["debug"]], {
      browser()
    }, ignoreNULL = TRUE)
    
    input_field_vals <- reactiveValues()
    
    # Parameters ----
    observeEvent(modeldat(),{
      parameter_names <- modeldat() %>% 
        get_required(type = type)

      
      lapply(parameter_names,
             function(parname_i){
               field_label <- tooltip_text(mytext = parname_i, 
                                           tooltip = get_parameter_info(
                                             model_ = neofm::get_model_name(modeldat()), 
                                             parameter_ = parname_i, 
                                             type_ = "description"
                                           )
               )
               field_return <- mod_auto_input_field_server(
                 id = parname_i, 
                 label = field_label,
                 value = modeldat() %>% 
                   slot(type) %>% 
                   .[[parname_i]],
                 datatype = "numerical"
               )
               input_field_vals[[parname_i]] <- field_return
             })
    }) # end of observeEvent
    
    
    # Render input fields
    output[["all_fields"]] <- renderUI({
      parameter_names <- modeldat() %>% 
        get_required(type = type)
      
      if (type == "param"){
        parameter_names_by_group <- group_parameters(parameter_names, model_ = get_model_name(modeldat()))
        
        collapse_panel_list <- lapply(names(parameter_names_by_group), function(p_group){
          
          parameter_names_expert <- expert_parameters(parameter_names_by_group[[p_group]],
                                                      model_ = get_model_name(modeldat()))[["yes"]]
          parameter_names_nonexpert <- expert_parameters(parameter_names_by_group[[p_group]],
                                                      model_ = get_model_name(modeldat()))[["no"]]
          
          
          input_fields_nonexpert <- tags$div(
            do.call(tagList,
                    lapply(parameter_names_nonexpert, function(parname_i){
                      mod_auto_input_field_ui(ns(parname_i))
                    })
            ),
            class = "inputfields_flexbox"
          )
          input_fields_expert <- tags$div(
            do.call(tagList,lapply(parameter_names_expert, function(parname_i){
              mod_auto_input_field_ui(ns(parname_i))
            })
            ),
            class = "inputfields_flexbox"
          )
          if (length(parameter_names_expert)>0){
            expert_collapse_panel <- shinyBS::bsCollapse(
              id = paste0(p_group,"_expert"),
              shinyBS::bsCollapsePanel(title = "expert", input_fields_expert)
            )
          } else {
            expert_collapse_panel <- NULL
          }
 
          
          panel_content <- tagList(
            input_fields_nonexpert,
            expert_collapse_panel
          )
          
          box(title = group_title_with_icon(p_group), 
              panel_content, 
              collapsed = TRUE, 
              collapsible = TRUE)
        })
        

        fluidRow(do.call(tagList, collapse_panel_list))
        
        
      } else if (type == "init"){
        
        input_fields <- lapply(parameter_names, function(parname_i){
          mod_auto_input_field_ui(ns(parname_i))
        }
        )
        
        div(do.call(tagList, input_fields), class = "inputfields_flexbox")
        
        
      }
      
    })

    return(input_field_vals)
    
  })
}
    
## To be copied in the UI
# mod_input_fields_ui("parameter_input_fields_1")
    
## To be copied in the server
# mod_input_fields_server("parameter_input_fields_1")
