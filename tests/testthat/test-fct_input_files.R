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
  
  # expect error if imported data is wrong format
  filepath2 <- system.file("extdata/parameter_descriptions.csv",
                           package = "cvasi.ui")
  expect_error(import_forcings(filepath2))
})

