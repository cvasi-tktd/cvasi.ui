test_that("mode_lookup valid identifiers", {
  for(i in seq(1, nrow(model_lookup)))
  {
    row <- model_lookup[i,]

    # find constructor    
    f <- get(row$model_f)
    expect_true(is.function(f))
    expect_in(environmentName(environment(f)), c("cvasi", "cvasi.ui"))
    
    # invoke constructor
    sc <- f()
    expect_true(cvasi::is_scenario(sc))
    expect_equal(sc@name, row$model_name)
    expect_equal(class(sc), row$scenario, ignore_attr=TRUE)
  }
})
