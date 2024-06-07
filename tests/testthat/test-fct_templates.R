test_that("template_exposure works", {
  # simple empty df ----
  o1 <- template_exposure()
  expected_o1 <- structure(list(time = NA, conc = NA), 
                           class = "data.frame", row.names = c(NA,-1L))
  expect_equal(o1, expected_o1)
  
  # simple empty df with trial column ----
  o2 <- template_exposure(trials = TRUE)
  expected_o2 <- structure(list(time = NA, conc = NA, trial = NA), 
                           class = "data.frame", row.names = c(NA,-1L))
  expect_equal(o2, expected_o2)
  
  # df with data ----
  o3 <- template_exposure(example_data = TRUE)
  expected_o3 <- structure(list(time = 0:21, 
                                conc = c(1, 0.95, 0.9025, 0.8574, 
                                         0.8145, 0.7738, 0.7351, 0.6983, 
                                         0.6634, 0.6302, 0.5987, 0.5688, 
                                         0.5404, 0.5133, 0.4877, 0.4633, 
                                         0.4401, 0.4181, 0.3972, 0.3774, 
                                         0.3585, 0.3406)
  ), class = "data.frame", row.names = c(NA, -22L))
  
  expect_equal(o3, expected_o3)
  
})

test_that("template_forcings works", {
  # simple empty df ----
  o1 <- template_forcings(forcings = c("rad","temp"))
  expected_o1 <- structure(list(
    time = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 
             10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 0L, 
             1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 
             15L, 16L, 17L, 18L, 19L, 20L, 21L), 
    forcing = c("rad", "rad", 
                "rad", "rad", "rad", "rad", "rad", "rad", "rad", "rad", "rad", 
                "rad", "rad", "rad", "rad", "rad", "rad", "rad", "rad", "rad", 
                "rad", "rad", "temp", "temp", "temp", "temp", "temp", "temp", 
                "temp", "temp", "temp", "temp", "temp", "temp", "temp", "temp", 
                "temp", "temp", "temp", "temp", "temp", "temp", "temp", "temp"
    ), value = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    )), class = "data.frame", row.names = c(NA, -44L))
  expect_equal(o1, expected_o1)
})
