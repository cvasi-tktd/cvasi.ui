test_that("toHTML works", {
  s <- c("This ", "is not ", "a *test*")
  o <- toHTML(s)
  
  expect_equal(class(o), c("html", "character"))
  expect_true(o == "<p>This is not a <em>test</em></p>\n")
  
  expect_error(toHTML(1:10))
})


test_that("read_roxygen works", {
  expect_equal(read_roxygen("base", "data.frame", print_tags_only = TRUE), 
               c("\\title", "\\name", "\\alias", "\\keyword", "\\keyword",
                 "\\description", "\\usage", "\\arguments", "\\details",
                 "\\value", "\\note", "\\references", "\\seealso", "\\examples"
               ))
  
  rox <- read_roxygen(package = "base",
                      f_name = "data.frame",
                      tag = "\\details")
  expect_equal(class(rox), c("html", "character"))
  
  expect_error(
    suppressWarnings(
      suppressMessages(
        read_roxygen(
          package = "somepackagename",
          f_name = "data.frame",
          tag = "\\title"
        )
      )
    ))
  
  
  model_titles <- lapply(setNames(cvasiUI::model_choices,cvasiUI::model_choices), function(x){
    read_roxygen(package = "cvasi",
                 f_name = x,
                 tag = "\\title")
  })
  
  expect_true(
    all(
      c(
        grepl("Lemna.*Schmitt", model_titles[["Lemna_Schmitt"]]),
        grepl("Myriophyllum model.*exponential", model_titles[["Myrio"]]),
        grepl("Myriophyllum model.*logistic", model_titles[["Myrio_log"]]),
        grepl("Algae.*exponential.*forcings", model_titles[["Algae_Weber"]]),
        grepl("Lemna.*Klein", model_titles[["Lemna_SETAC"]]),
        grepl("Algae.*exponential.*without.*forcings", model_titles[["Algae_Simple"]])
      )))
  
})
