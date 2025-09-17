test_that("construct_model works", {
  
  #o <- construct_model("GUTS_RED_IT")
  #expect_equal(class(o), structure("GutsRedIt", package = "cvasi"))
  #expect_true(inherits(o, "EffectScenario"))
  
  o <- construct_model("Lemna_Schmitt")
  expect_equal(class(o), structure("LemnaSchmitt", package = "cvasi"))
  expect_true(inherits(o, "EffectScenario"))
  
  o <- construct_model("Algae_Weber")
  expect_equal(class(o), structure("AlgaeWeber", package = "cvasi"))
  expect_true(inherits(o, "EffectScenario"))
  
  expect_error(construct_model("sum"))
  expect_error(construct_model("nofunction"))
  
})

test_that("get_required works - Algae_Weber", {
  dat <- construct_model("Algae_Weber")
  p_ <- get_required(dat, type = "param")
  i_ <- get_required(dat, type = "init")
  f_ <- get_required(dat, type = "forcings")
  
  p_expected <- c("mu_max", "m_max", "v_max", "k_s",
                  "Q_min", "Q_max", "R_0", "D",
                  "T_opt", "T_min", "T_max", "I_opt",
                  "EC_50", "b")
  i_expected <- c("A", "Q", "P")
  f_expected <- c("T_act","I")
  
  expect_equal(p_, p_expected)
  expect_equal(i_, i_expected)
  expect_equal(f_, f_expected)
})

test_that("get_required works - magma", {
  dat <- construct_model("Magma_exp")
  p_ <- get_required(dat, type = "param")
  i_ <- get_required(dat, type = "init")
  f_ <- get_required(dat, type = "forcings")
  
  # k_photo_max only included for backwards compatibility of the parameters
  p_expected <- c("mu_control", "E_max", "EC50_int", "b", "P", "r_A_DW",
                  "r_FW_DW", "r_FW_V", "r_DW_TSL", "K_pw", "k_met")
  i_expected <- c("BM", "M_int")
  f_expected <- character(0)
  
  expect_equal(sort(p_), sort(p_expected))
  expect_equal(i_, i_expected)
  expect_equal(f_, f_expected)
})

test_that("get_required throws error", {
  expect_error(get_required(1:10, type = "param"))
  
  expect_error(get_required(
    cvasi::Magma(),
    type = "typenotsupported")
  )
})

test_that("get_val works - Lemna_Schmitt", {
  dat <- construct_model("Lemna_Schmitt")
  p_ <- get_val(dat, type = "param")
  i_ <- get_val(dat, type = "init")
  f_ <- get_val(dat, type = "forcings")
  
  p_expected <- list(Emax=1, AperBM=1000, Kbm=1, P_Temp=FALSE,
                     MolWeight=381, k_phot_fix=TRUE, k_phot_max=0.42, k_resp=0.0, k_loss=0.0,
                     Tmin=8.0, Tmax=40.5, Topt=26.7, t_ref=25, Q10=2, k_0=3, a_k=5E-5, C_P=0.3,
                     CP50=0.0043, a_P=1, KiP=101, C_N=0.6, CN50=0.034, a_N=1, KiN=604, BM50=176,
                     mass_per_frond=0.0001, BMw2BMd=16.7, EC50=0.3, b = 4.16, P_up = 0.0054)
  i_expected <- c(BM=0.0012, E=1, M_int=0)

  expect_equal(p_[sort(names(p_))], p_expected[sort(names(p_expected))])
  expect_equal(i_, i_expected)

})

test_that("get_val throws error", {
  expect_error(get_val(1:10, type = "param"))
  
  expect_error(get_val(
    cvasi::Magma(),
    type = "typenotsupported")
  )
})

test_that("forcings_required works", {
  expect_true(forcings_required(cvasi::Lemna_Schmitt()))
  expect_false(forcings_required(Magma_exp()))
  expect_false(forcings_required(Magma_log()))
  expect_true(forcings_required(cvasi::Algae_Weber()))
  
  expect_error(forcings_required("Lemna_Schmitt"))
  
})


test_that("pars_range works", {
  all_models <- lapply(model_choices, function(mod){
    mod %>% 
      construct_model() %>% 
      pars_range()
  }) %>%
    dplyr::bind_rows()
  
  expect_equal(
    all_models$in_range,
    !is.na(all_models$value)
  )
})


test_that("check_model_complete works", {
  expect_true(check_model_complete(cvasi::metsulfuron, type = "param"))
  expect_true(check_model_complete(cvasi::metsulfuron, type = "init"))
  expect_true(check_model_complete(cvasi::metsulfuron, type = "forcings"))
  
  # error when 'type' is misspelled or not supported
  expect_error(check_model_complete(cvasi::metsulfuron, type = "forcing"))
  
  # return TRUE if the 'type' is not required but with a warning
  expect_warning(
    grs_out <- check_model_complete(cvasi::GUTS_RED_SD(), type = "forcings")
  )
  expect_true(grs_out)
})

test_that("check_exposure_complete", {
  exp_dat <- cvasi::schmitt2013  %>% 
    dplyr::select(time, trial, conc)
  expect_true(check_exposure_complete(exp_dat))
  
  expect_false(cvasi::schmitt2013  %>% 
                 dplyr::select(t=time, trial, conc) %>% 
                 check_exposure_complete()
               )
  expect_false(check_exposure_complete(1:10))
})
