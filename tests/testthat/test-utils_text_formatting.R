test_that("field_label works", {
  expect_equal(field_label("name", "µg/L"), "name [µg/L]")
  expect_equal(field_label("name", NA), "name")
  expect_equal(field_label("name", "logical"), "name [-]")
  expect_equal(field_label("name", ""), "name")
})


test_that("tooltip_text works", {
  t1 <- "Tmin"
  t2 <- "The tooltip text that appears when the mouse hovers over `mytext`"
  
  x1 <- tooltip_text(mytext = t1,
                     tooltip = t2)
  expect_equal(class(x1), c("html", "character"))
  expect_true(grepl(paste0(">",t1,"<"), x1))
  expect_true(grepl(paste0("<div title='",t2,"'>"),x1))
  
  x2 <- tooltip_text(mytext = "Tmin")
  expect_equal(class(x2), c("html", "character"))
  expect_true(grepl(paste0("<div>",t1,"</div>"),x2))
  
})
