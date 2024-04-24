test_that("app_ui works", {
  ui <- app_ui()
  expect_s3_class(ui, "shiny.tag")
})
