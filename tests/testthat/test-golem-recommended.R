test_that("app server", {
  server <- app_server
  expect_type(server, "closure")
  # Check that formals have not been removed
  fmls <- formals(app_server)
  for (i in c("input", "output", "session")) {
    expect_true(i %in% names(fmls))
  }
})

test_that(
  "app_sys works",
  {
    expect_true(
      app_sys("golem-config.yml") != ""
    )
  }
)

test_that(
  "golem-config works",
  {
    config_file <- app_sys("golem-config.yml")
    skip_if(config_file == "")

    expect_true(
      get_golem_config(
        "app_prod",
        config = "production",
        file = config_file
      )
    )
    expect_false(
      get_golem_config(
        "app_prod",
        config = "dev",
        file = config_file
      )
    )
  }
)

test_that("app ui", {
  ui <- app_ui()
  #golem::expect_shinytaglist(ui)
  golem::expect_shinytag(ui)
  # Check that formals have not been removed
  fmls <- formals(app_ui)
  for (i in c("request")) {
    expect_true(i %in% names(fmls))
  }
})

testthat::skip("needs further evaluation")

# Configure this test to fit your need.
# testServer() function makes it possible to test code in server functions and modules, without needing to run the full Shiny application
testServer(app_server, {

  # Set and test an input
  session$setInputs(`prediction_workflow-active_model-selectized` = "Myrio")
  expect_equal(input[["active_model"]], "Myrio")

  # Example of tests you can do on the server:
  # - Checking reactiveValues
  # expect_equal(r$lg, 'EN')
  # - Checking output
  # expect_equal(output$txt, "Text")
})

testthat::skip("golem::expect_running works only if the shiny app is started with 'run_app()'")
# Configure this test to fit your need
test_that(
  "app launches",
  {
    golem::expect_running(sleep = 5)
  }
)
