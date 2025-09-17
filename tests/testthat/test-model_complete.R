test_that("validate model parameter defaults available", {
  
  o <- lapply(cvasi.ui::model_choices, function(x){
    p_expected <- cvasi.ui::model_defaults[[x]][["parameter_defaults"]] %>% names
    p_ <- x %>% 
      construct_model() %>% 
      get_required(type = "param")

    all(p_expected %in% p_)
  })
  
  expected <- as.list(rep(TRUE, length(cvasi.ui::model_choices)))
  names(expected) <- cvasi.ui::model_choices
  names(o) <- cvasi.ui::model_choices
  
  expect_equal(o, expected)

})


test_that("validate model initial value defaults available", {
  o <- lapply(cvasi.ui::model_choices, function(x){
    i_expected <- cvasi.ui::model_defaults[[x]][["init_defaults"]] %>% names
    i_ <- x %>% 
      construct_model() %>% 
      get_required(type = "init")
    
    all(i_expected %in% i_)
  })
  
  expected <- as.list(rep(TRUE, length(cvasi.ui::model_choices)))
  names(expected) <- cvasi.ui::model_choices
  names(o) <- cvasi.ui::model_choices
  
  expect_equal(o, expected)
})


test_that("validate model forcing defaults available", {
  o <- lapply(cvasi.ui::model_choices, function(x){
    i_expected <- cvasi.ui::model_defaults[[x]][["forcing_defaults"]] %>% names
    i_ <- x %>% 
      construct_model() %>% 
      get_required(type = "forcing")
    
    all(i_expected %in% i_)
  })
  
  expected <- as.list(rep(TRUE, length(cvasi.ui::model_choices)))
  names(expected) <- cvasi.ui::model_choices
  names(o) <- cvasi.ui::model_choices
  
  expect_equal(o, expected)
})
