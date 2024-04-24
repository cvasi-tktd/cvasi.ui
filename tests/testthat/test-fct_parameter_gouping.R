test_that("group_parameters works", {
  
  p <- cvasi::Lemna_Schmitt() %>% 
    cvasiUI:::get_required("param")
  m <- "Lemna_Schmitt"
  o <- group_parameters(p = p,
                        model_ = m)
  expect_equal(names(o), c("physiological", "toxicodynamic", "toxicokinetic"))
  
  
  expect_error(group_parameters(p = 1:3, model_ = m))
  expect_error(group_parameters(p = p, model_ = c("Lemna_Schmitt", "Myriophyllum")))
  
  p <- cvasi::Myrio() %>% 
    cvasiUI:::get_required("param")
  m <- "Myriophyllum"
  o <- group_parameters(p = p,
                        model_ = m)
  expect_equal(names(o), c("physiological", "toxicodynamic", "toxicokinetic"))
  
  p <- cvasi::Algae_Weber() %>% 
    cvasiUI:::get_required("param")
  m <- "Algae_Weber"
  o <- group_parameters(p = p,
                        model_ = m)
  expect_equal(names(o), c("physiological", "toxicodynamic", "toxicokinetic"))
  
  lapply(setNames(model_choices, model_choices), function(m){
    x <- cvasiUI:::construct_model(m)
    p <- x %>% 
      cvasiUI:::get_required("param")
    m <- cvasi::get_model_name(x)
    o <- group_parameters(p = p,
                          model_ = m)
    expect_equal(names(o), c("physiological", "toxicodynamic", "toxicokinetic"))
  })
  
})


test_that("expert_parameters works", {
  
  p <- cvasi::Lemna_Schmitt() %>% 
    cvasiUI:::get_required("param")
  m <- "Lemna_Schmitt"
  o <- expert_parameters(p = p,
                        model_ = m)
  expect_equal(names(o), c("no", "yes"))
  
  
  expect_error(expert_parameters(p = 1:3, model_ = m))
  expect_error(expert_parameters(p = p, model_ = c("Lemna_Schmitt", "Myriophyllum")))
  
  p <- cvasi::Myrio() %>% 
    cvasiUI:::get_required("param")
  m <- "Myriophyllum"
  o <- expert_parameters(p = p,
                        model_ = m)
  expect_equal(names(o), c("no", "yes"))
  
  p <- cvasi::Algae_Weber() %>% 
    cvasiUI:::get_required("param")
  m <- "Algae_Weber"
  o <- expert_parameters(p = p,
                        model_ = m)
  expect_equal(names(o), c("no"))
  
})

test_that("group_title_with_icon works", {
  
  g <- cvasiUI::parameter_descriptions %>% 
    dplyr::filter(group != "state variable") %>% 
    dplyr::pull(group) %>% 
    unique()
  
  lapply(g, function(i) {
    expect_equal(class(group_title_with_icon(i)), "shiny.tag")
  })
  
  x1 <- "a char"
  expect_equal(group_title_with_icon(x1),x1)
  x2 <- 1
  expect_error(group_title_with_icon(x2))
  x3 <- c("a", "b")
  expect_error(group_title_with_icon(x3))
})
