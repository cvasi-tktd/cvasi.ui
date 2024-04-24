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
  
  
  expected_cvasi_model_titles <- list(structure("<p>Lemna model (Schmitt et al. 2013)</p>\n", html = TRUE, class = c("html", "character")), 
       structure("<p>Myriophyllum model with exponential growth</p>\n", html = TRUE, class = c("html", "character")), 
       structure("<p>Myriophyllum model with logistic growth</p>\n", html = TRUE, class = c("html", "character")), 
       structure("<p>Algae model with exponential growth and forcings (I, T)</p>\n", html = TRUE, class = c("html", "character")))
  
  cvasi_model_titles <- lapply(cvasiUI::model_choices, function(x){
             read_roxygen(package = "cvasi",
               f_name = x,
               tag = "\\title")
  })
  
  expect_equal(cvasi_model_titles, expected_cvasi_model_titles)
})
