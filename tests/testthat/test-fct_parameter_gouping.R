test_that("group_parameters works", {
  
  expected_group_names <- c("toxicodynamic", "toxicokinetic", "physiological")
  expected_group_names_tktd <- c("toxicodynamic", "toxicokinetic", "physiological", "degradation")
  expected_group_names_weber <- c("toxicodynamic", "physiological", "degradation")
  p <- cvasi::Lemna_Schmitt() %>% 
    cvasi.ui:::get_required("param")
  m <- "Lemna_Schmitt"
  o <- group_parameters(p = p,
                        model_ = m)
  expect_equal(names(o), expected_group_names)
  
  
  expect_error(group_parameters(p = 1:3, model_ = m))
  expect_error(group_parameters(p = p, model_ = c("Lemna_Schmitt", "Myriophyllum")))
  
  
  p <- cvasi::Myrio() %>% 
    cvasi.ui:::get_required("param")
  m <- "Myriophyllum"
  o <- group_parameters(p = p,
                        model_ = m)
  expect_equal(names(o), expected_group_names)
  
  p <- cvasi::Algae_Weber() %>% 
    cvasi.ui:::get_required("param")
  m <- "Algae_Weber"
  o <- group_parameters(p = p,
                        model_ = m)
  expect_equal(names(o), expected_group_names_weber)
  
  lapply(setNames(model_choices, model_choices), function(m){
    x <- cvasi.ui:::construct_model(m)
    p <- x %>%
      cvasi.ui:::get_required("param")
    m <- cvasi::get_model_name(x)
    o <- group_parameters(p = p,
                          model_ = m)
    #print(cvasi::get_model_name(x))
    if(cvasi::get_model_name(x) == "Algae_Weber"){
      expected_group_names_ <- expected_group_names_weber
    }else if(cvasi::get_model_name(x) == "Algae_TKTD"){
      expected_group_names_ <- expected_group_names_tktd
    }else{
      expected_group_names_ <- expected_group_names
    }
    expect_equal(names(o), expected_group_names_)
  })
  
})


test_that("expert_parameters works", {
  
  p <- cvasi::Lemna_Schmitt() %>% 
    cvasi.ui:::get_required("param")
  m <- "Lemna_Schmitt"
  o <- expert_parameters(p = p,
                         model_ = m)
  expect_equal(names(o), c("no", "yes"))
  
  
  expect_error(expert_parameters(p = 1:3, model_ = m))
  expect_error(expert_parameters(p = p, model_ = c("Lemna_Schmitt", "Myriophyllum")))
  
  p <- cvasi::Myrio() %>% 
    cvasi.ui:::get_required("param")
  m <- "Myriophyllum"
  o <- expert_parameters(p = p,
                         model_ = m)
  expect_equal(names(o), c("no", "yes"))
  
  p <- cvasi::Algae_Weber() %>% 
    cvasi.ui:::get_required("param")
  m <- "Algae_Weber"
  o <- expert_parameters(p = p,
                         model_ = m)
  expect_equal(names(o), c("no"))
  
})

test_that("group_title_with_icon works", {
  
  g <- cvasi.ui::parameter_descriptions %>% 
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



test_that("order_parameter_groups works", {
  p_order <- c("toxicodynamic", "toxicokinetic", "physiological")
  
  parameter_groups <- list(
    physiological = c("k_phot_fix", "k_phot_max", "k_resp",
                      "k_loss", "Tmin", "Tmax", "Topt", "t_ref", "Q10", "k_0", "a_k",
                      "C_P", "CP50", "a_P", "KiP", "C_N", "CN50", "a_N", "KiN", "BM50",
                      "mass_per_frond", "BMw2BMd"),
    toxicodynamic = c("Emax", "EC50", "b"),
    toxicokinetic = c("P_up", "AperBM", "Kbm", "P_Temp", "MolWeight"))
  
  # list is same length as p_order
  o1 <- order_parameter_groups(parameter_groups, p_order)
  expect_equal(names(o1), p_order)
  
  # list is shorter than p_order
  o2 <- order_parameter_groups(parameter_groups[1:2], p_order)
  expect_equal(names(o2), c("toxicodynamic", "physiological"))
  
})
