test_that("list_rbind2 works", {
  
  set.seed(123)
  
  # works if data.frames are equally sized
  x1 <- list(a = data.frame(time = 1:3, value = rnorm(3)),
             b = data.frame(time = 1:3, value = rnorm(3)),
             c = data.frame(time = 1:3, value = rnorm(3)))
  res_x1 <- list_rbind2(x1, listnames_to = "my_factor")
  expected_x1 <- structure(list(
    time = c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L), 
    value = c(-0.560475646552213, -0.23017748948328, 1.55870831414912,
              0.070508391424576, 0.129287735160946, 1.71506498688328,
              0.460916205989202, -1.26506123460653, -0.686852851893526), 
    my_factor = c("a", "a", "a", "b", "b", "b", "c", "c", "c")), 
    row.names = c(NA, -9L), class = "data.frame")
  expect_equal(res_x1, expected_x1)
  expect_true( all(do.call(c, lapply(x1, names)) %>% unique() %in% names(res_x1)))
  
  # error if list is not completely named 
  x2 <- list(a = data.frame(time = 1:3, value = rnorm(3)),
             data.frame(time = 1:3, value = rnorm(3)),
             data.frame(time = 1:3, value = rnorm(3)))
  expect_error(list_rbind2(x2, listnames_to = "my_factor"))
  
  # works if the data.frames have different length
  x3 <- list(a = data.frame(time = 1:3, value = rnorm(3)),
             b = data.frame(time = 1:5, value = rnorm(5)),
             c = data.frame(time = 1:2, value = rnorm(2)))
  res_x3 <- list_rbind2(x3, listnames_to = "my_factor")
  expected_x3 <- structure(list(
    time = c(1L, 2L, 3L, 1L, 2L, 3L, 4L, 5L, 1L, 2L), 
    value = c(0.701355901563686, -0.472791407727934, -1.06782370598685, 
              -0.217974914658295, -1.02600444830724, -0.72889122929114, 
              -0.625039267849257, -1.68669331074241, 0.837787044494525, 
              0.153373117836515), 
    my_factor = c("a","a", "a", "b", "b", "b", "b", "b", "c", "c")), 
    row.names = c(NA,-10L), class = "data.frame")
  expect_equal(res_x3, expected_x3)
  expect_true( all(do.call(c, lapply(x3, names)) %>% unique() %in% names(res_x3)))
  
  # works if the data.frames have different width (different columns)
  x4 <- list(a = data.frame(t = 1:3, value = rnorm(3), value2 = letters[1:3]),
             b = data.frame(time = 1:5, value = rnorm(5)),
             c = data.frame(time = 1:2, value = rnorm(2)))
  res_x4 <- list_rbind2(x4, listnames_to = "my_factor")
  expected_x4 <- structure(list(t = c(1L, 2L, 3L, NA, NA, NA, NA, NA, NA, NA), 
                                value = c(-1.13813693701195, 1.25381492106993, 0.426464221476814, 
                                          -0.295071482992271, 0.895125661045022, 0.878133487533042, 
                                          0.821581081637487, 0.688640254100091, 0.553917653537589, 
                                          -0.0619117105767217), 
                                value2 = c("a", "b", "c", NA, NA, NA, NA, NA, NA, NA), 
                                my_factor = c("a", "a", "a", "b", "b", "b", "b", "b", "c", "c"), 
                                time = c(NA, NA, NA, 1L, 2L, 3L, 4L, 5L, 1L, 2L)), 
                           row.names = c(NA, -10L), class = "data.frame")
  expect_equal(res_x4, expected_x4)
  expect_true( all(do.call(c, lapply(x4, names)) %>% unique() %in% names(res_x4)))
})


test_that("is_named_list works", {
  x1 <- list(a = 1:10, b = 2:5, c = 3)
  expect_true(is_named_list(x1))
  
  x2 <- list(1:10, 2:5, 3)
  expect_false(is_named_list(x2))
  
  x3 <- list(a= 1:10, 2:5, 3)
  expect_false(is_named_list(x3))
  
  x4 <- data.frame(a = 1:10, b = letters[1:10], c = LETTERS[1:10])
  expect_error(is_named_list(x4))
  
  x5 <- "text"
  expect_error(is_named_list(x5))
  
  x6 <- 1:10
  expect_error(is_named_list(x6))
})

