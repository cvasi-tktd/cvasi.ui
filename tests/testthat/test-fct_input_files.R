test_that("check_forcings_ts works", {

  f_dat <- list(
    rad = data.frame(time = 1:10, value = rnorm(10)),
    temp = data.frame(time = 1:10, value = rnorm(10))
  )
  
  exp_f <- c("rad", "temp")
  expect_equal(check_forcings_ts(f_dat, exp_f), NULL)
  
  exp_f <- c("irr", "tmp")
  expect_error(check_forcings_ts(f_dat, exp_f))
 
})


test_that("import_forcings works", {
  
  filepath <- system.file("extdata/forcings.csv", package = "cvasi.ui")
  forc <- import_forcings(filepath)
  
  expect_equal(class(forc), "list")
  expect_equal(names(forc), c("rad", "temp"))
  expect_equal(class(forc[[1]]), "data.frame")
  expect_equal(names(forc[[1]]), c("time", "value"))
  expect_equal(class(forc[[2]]), "data.frame")
  expect_equal(names(forc[[2]]), c("time", "value"))
  
  # expect error when no file is loaded
  expect_error(import_forcings("nofile"))
})


test_that("parameter_descriptions format", {
  expect_true(all(is.numeric(parameter_descriptions$lower.boundary)))
  expect_true(all(is.numeric(parameter_descriptions$upper.boundary)))
  # all values present?
  expect_false(any(is.na(parameter_descriptions$model)))
  expect_false(any(is.na(parameter_descriptions$parameter)))
  expect_false(any(is.na(parameter_descriptions$description)))
  expect_false(any(is.na(parameter_descriptions$unit)))
  expect_false(any(is.na(parameter_descriptions$group)))
  expect_false(any(is.na(parameter_descriptions$expert.value)))
  expect_false(any(is.na(parameter_descriptions$default)))
  expect_false(any(is.na(parameter_descriptions$lower.boundary)))
  expect_false(any(is.na(parameter_descriptions$upper.boundary)))
})
