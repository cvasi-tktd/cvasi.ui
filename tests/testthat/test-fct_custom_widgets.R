testthat::test_that("box_left and dashboardbox are equal",{
  expect_equal(
    dashboardbox_left(title = "mytitle"),
    shinydashboard::box(title = "mytitle")
  )
  
  expect_equal(
    dashboardbox_left(),
    shinydashboard::box()
  )

})

testthat::test_that("new dashboard box produces correct class, title, contents, ...",{
  box_title <- "mytitle"
  x <- dashboardbox_left(title = box_title, 
                status = "primary",
                width = 12, 
                collapsed = TRUE,
                collapsible = TRUE)
  
  expect_equal(
    htmltools::tagQuery(x)$find("h3")$selectedTags()[[1]]$attribs$class,
    "box-title"
  )

  expect_equal(
    htmltools::tagQuery(x)$find("h3")$selectedTags()[[1]]$children[[1]],
    box_title
  )
  
  expect_true(
    length(htmltools::tagQuery(x)$find(".box-body")$selectedTags())>0
  )
  
  #____________
  x <- dashboardbox_left(title = box_title, 
                         collapsible = TRUE)
  
  expect_equal(
    htmltools::tagQuery(x)$find("h3")$selectedTags()[[1]]$attribs$class,
    "box-title"
  )
  
  expect_equal(
    htmltools::tagQuery(x)$find("h3")$selectedTags()[[1]]$children[[1]],
    box_title
  )
  
  expect_true(
  length(htmltools::tagQuery(x)$find(".box-body")$selectedTags())>0
  )

  
})

