#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  #tagList(
  # Leave this function for adding external resources
  #golem_add_external_resources(),
  # Your application UI logic
  shinydashboard::dashboardPage(
    #skin = "green",
    title = "cvasiUI",
    shinydashboard::dashboardHeader(title = "cvasiUI",
                                    tags$li(class = "dropdown",
                                            actionLink("guide", icon("question-circle")),
                                            title = "Tutorial"
                                    ),
                                    tags$li(class = "dropdown",
                                            actionLink("expand_boxes", icon("expand-arrows-alt")),
                                            title = "Expand dashboard boxes"
                                    ),
                                    tags$li(class = "dropdown",
                                            actionLink("collapse_boxes", icon("compress-arrows-alt")),
                                            title = "Collapse dashboard boxes"
                                    )
    ),
    shinydashboard::dashboardSidebar(disable = TRUE, collapsed = TRUE#,
                                     # shinydashboard::sidebarMenu(
                                     #   shinydashboard::menuItem("Prediction", tabName = "prediction"),
                                     #   shinydashboard::menuItem("Calibration", tabName = "calibration"),
                                     #   shinydashboard::menuItem("Validation", tabName = "validation")
                                     # )
    ),
    shinydashboard::dashboardBody(
      golem_add_external_resources(),
      # shinydashboard::tabItems(
      # shinydashboard::tabItem("prediction",
      mod_prediction_workflow_ui("prediction_workflow"),  
      # ),
      # shinydashboard::tabItem("calibration",""),
      # shinydashboard::tabItem("validation","")
      # )
      
    )
  )
  #)
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "cvasiUI"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(),
    cicerone::use_cicerone()
  )
}
