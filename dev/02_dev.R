# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()
usethis::use_package("cvasi")

# during dev use
#desc::desc_add_remotes("local::../cvasi")
#desc::desc_clear_remotes() # clears all remotes links
#desc::desc_get_remotes()

desc::desc_add_remotes("github::cvasi-tktd/cvasi")



## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "prediction_workflow", with_test = FALSE) # Name of the module
golem::add_module(name = "parameter_input", with_test = FALSE) # Name of the module
golem::add_module(name = "init_input", with_test = FALSE) # Name of the module
golem::add_module(name = "auto_input_field", with_test = FALSE) # Name of the module
golem::add_module(name = "input_fields", with_test = FALSE) # Name of the module
golem::add_module(name = "exposure_input", with_test = FALSE) # Name of the module
golem::add_module(name = "exposuretable_input", with_test = FALSE) # Name of the module
golem::add_module(name = "exposurefile_input", with_test = FALSE) # Name of the module
golem::add_module(name = "forcings_input", with_test = FALSE) # Name of the module
golem::add_module(name = "epx_mtw_settings", with_test = FALSE) # Name of the module
golem::add_module(name = "prediction", with_test = FALSE) # Name of the module


## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("model_helpers", with_test = TRUE)
golem::add_fct("input_files", with_test = TRUE)
golem::add_fct("parameter_info", with_test = TRUE)
golem::add_fct("parameter_gouping", with_test = TRUE)
golem::add_fct("custom_widgets", with_test = FALSE)
golem::add_fct("guide", with_test = FALSE)
golem::add_fct("js", with_test = FALSE)
golem::add_fct("templates", with_test = FALSE)
golem::add_utils("text_formatting", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)
golem::add_utils("model_description", with_test = FALSE)


## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("shinydashboard_box_update")
#golem::add_js_handler("handlers")
golem::add_css_file("generic")
golem::add_css_file("parameters")
golem::add_css_file("check_input")
golem::add_css_file("dashboard")
golem::add_css_file("cicerone")
#golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "param_descr", open = TRUE)
usethis::use_data_raw(name = "model_defaults", open = TRUE)

## Tests ----
## Add one line by test you want to create
#usethis::use_test("app")

# Documentation

## Vignette ----
#usethis::use_vignette("cvasi.ui")
#devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
#usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
#covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
#usethis::use_github()

# GitHub Actions
#usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
#usethis::use_github_action_check_release()
#usethis::use_github_action_check_standard()
#usethis::use_github_action_check_full()
# Add action for PR
#usethis::use_github_action_pr_commands()

# Travis CI
#usethis::use_travis()
#usethis::use_travis_badge()

# AppVeyor
#usethis::use_appveyor()
#usethis::use_appveyor_badge()

# Circle CI
#usethis::use_circleci()
#usethis::use_circleci_badge()

# Jenkins
#usethis::use_jenkins()

# GitLab CI
#usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
