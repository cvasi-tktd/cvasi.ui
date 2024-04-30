test_that("validate model parameter defaults available", {
  
  o <- lapply(setNames(cvasiUI::model_choices, cvasiUI::model_choices), function(x){
    p_expected <- cvasiUI::model_defaults[[x]][["parameter_defaults"]] %>% names
    p_ <- x %>% 
      construct_model() %>% 
      get_required(type = "param")

    all(p_expected %in% p_)
  })
  
  expected <- as.list(rep(TRUE, length(cvasiUI::model_choices)))
  names(expected) <- cvasiUI::model_choices
  
  expect_equal(o, expected)

})


test_that("validate model initial value defaults available", {
  o <- lapply(setNames(cvasiUI::model_choices, cvasiUI::model_choices), function(x){
    i_expected <- cvasiUI::model_defaults[[x]][["init_defaults"]] %>% names
    i_ <- x %>% 
      construct_model() %>% 
      get_required(type = "init")
    
    all(i_expected %in% i_)
  })
  
  expected <- as.list(rep(TRUE, length(cvasiUI::model_choices)))
  names(expected) <- cvasiUI::model_choices
  
  expect_equal(o, expected)
})


test_that("validate model forcing defaults available", {
  o <- lapply(setNames(cvasiUI::model_choices, cvasiUI::model_choices), function(x){
    browser()
    i_expected <- cvasiUI::model_defaults[[x]][["forcing_defaults"]] %>% names
    i_ <- x %>% 
      construct_model() %>% 
      get_required(type = "forcing")
    
    all(i_expected %in% i_)
  })
  
  expected <- as.list(rep(TRUE, length(cvasiUI::model_choices)))
  names(expected) <- cvasiUI::model_choices
  
  expect_equal(o, expected)
})
