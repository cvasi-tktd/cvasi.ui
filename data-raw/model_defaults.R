## code to prepare `model_defaults` dataset goes here

# Model choices ----
model_choices <- list(`Lemna (Schmitt)` = "Lemna_Schmitt", 
                      `Lemna (SETAC)` = "Lemna_SETAC", 
                      `Generic macrophyte model (exponential)` = "Myrio", 
                      `Generic macrophyte model (logistic)` = "Myrio_log", 
                      `Algae (Weber)` = "Algae_Weber",
                      `Algae (simplified)` = "Algae_Simple")

usethis::use_data(model_choices, overwrite = TRUE)

# Model names, scenario/class names and constructor function names lookup table
all_model_dat <- lapply(setNames(cvasi.ui::model_choices,cvasi.ui::model_choices),
                        function(x){
                          x %>%
                            cvasi.ui:::construct_model()
                        })


model_lookup <- data.frame(
  do.call(rbind, lapply(all_model_dat, function(x) {
    data.frame(
      model_name = x %>% cvasi::get_model_name(), 
      scenario = x %>% class())
  }
  )),
  model_f = names(all_model_dat))
usethis::use_data(model_lookup, overwrite = TRUE)

## what are the default values the models are constructed with?
model_inputs <- lapply(setNames(model_choices,model_choices), function(m){
  x <- eval(parse(text = paste0("cvasi::",m)[[1]]))()
  list(param.req = x@param.req,
       forcings.req = x@forcings.req,
       init = x@init
  )
})

# Default exposure ----
default_exposure <- structure(list(time = 0:21, 
                                   conc = c(0.0446, 0.0182, 0.007, 0.0563, 
                                            0.1079, 0.047, 0.0193, 0.061, 
                                            0.0555, 0.163, 0.1277, 0.1587, 
                                            0.0269, 0.1438, 0.1004, 0.1563, 
                                            0.1138, 0.1461, 0.1101, 0.2733, 
                                            0.2752, 0.2772), 
                                   trial = c("T1", "T1", "T1", "T1", "T1",
                                             "T1", "T1", "T1", "T1", "T1",
                                             "T1", "T1", "T1", "T1", "T1",
                                             "T1", "T1", "T1", "T1", "T1",
                                             "T1", "T1")), 
                              row.names = c(NA, -22L),
                              class = "data.frame")
usethis::use_data(default_exposure, overwrite = TRUE)



# Default parameters ----
parameter_defaults <- list(
  GUTS_RED_SD = list(kd = 22, 
                     hb = 0.01,
                     z = 0.5, 
                     kk = 0.08), # From howto
  GUTS_RED_IT = list(kd=1.2, 
                     hb=0, 
                     alpha=9.2, 
                     beta=4.3), # From Manual
  Lemna_Schmitt = list(Emax = 0.784, 
                       AperBM = 1000, 
                       Kbm = 0.75, 
                       P_Temp = FALSE, 
                       MolWeight = 381, 
                       k_phot_fix = FALSE, 
                       k_phot_max = 0.47, 
                       k_resp = 0.05, 
                       k_loss = 0, 
                       Tmin = 8,
                       Tmax = 40.5, 
                       Topt = 26.7, 
                       t_ref = 25, 
                       Q10 = 2, 
                       k_0 = 3, 
                       a_k = 5e-05, 
                       C_P = 0.3, 
                       CP50 = 0.0043, 
                       a_P = 1, 
                       KiP = 101, 
                       C_N = 0.6, 
                       CN50 = 0.034,
                       a_N = 1, 
                       KiN = 604, 
                       BM50 = 176, 
                       mass_per_frond = 1e-04,
                       BMw2BMd = 16.7,
                       EC50 = 0.3, 
                       b = 4.16, 
                       P_up = 0.0054), # from metsulfuron()@param
  Lemna_SETAC = list(k_photo_fixed = 1,
                     k_photo_max = 0.42, 
                     k_loss = 0, 
                     BM_min = 0, 
                     T_opt = 26.7,
                     T_min = 8, 
                     T_max = 40.5, 
                     Q10 = 2, 
                     T_ref = 25, 
                     alpha = 5e-05, 
                     beta = 0.025, 
                     N_50 = 0.034, 
                     P_50 = 0.0043, 
                     BM_L = 176, 
                     E_max = 1, 
                     r_A_DW = 1000, 
                     r_FW_DW = 16.7, 
                     r_FW_V = 1, 
                     r_DW_FN = 4e-04, 
                     K_pw = 1, 
                     k_met = 0, 
                     EC50_int = 0.3, 
                     b = 4.16, 
                     P = 0.0054),
  Myrio = list(k_photo_max = 0.15, 
               E_max = 1, 
               r_A_DW = 1000,
               r_FW_DW = 16.7,
               r_FW_V = 1, 
               K_pw = 1, 
               k_met = 0, # from Myrio()@param
               EC50_int = 1, 
               b = 1, 
               P = 0.0001, 
               r_DW_TSL = 0.0001), 
  Myrio_log = list(k_photo_max = 0.15, 
                   E_max = 1, 
                   r_A_DW = 1000,
                   r_FW_DW = 16.7,
                   r_FW_V = 1,
                   K_pw = 1,
                   k_met = 0, # from Myrio_log()@param
                   EC50_int = 1,
                   b = 1,
                   P = 0.0001,
                   r_DW_TSL = 0.0001,
                   BM_L = 1000),
  Algae_Weber = list(mu_max=1.738, 
                     m_max=0.05, 
                     v_max=0.052, 
                     k_s=0.068, 
                     Q_min=0.0011, 
                     Q_max=0.0144, 
                     R_0=0.36, 
                     D=0.5, 
                     T_opt=27, 
                     T_min=0, 
                     T_max=35,
                     I_opt=120,
                     EC_50=115,
                     b=1.268,
                     k=0.2),
  Algae_Simple = list(mu_max=1.738,
                      EC_50=115,
                      b=1.268,
                      kD=0.1,
                      dose_response=0,
                      scaled=0,
                      const_growth=0),
  DEB_abj = list(p_M = NA, v = NA, k_J = NA,
                 p_Am = NA, kap = NA, E_G = NA,
                 f = NA, E_Hj = NA, E_Hp = NA, 
                 kap_R = NA, ke = NA, c0 = NA,
                 cT = NA, L_b = NA, L_j = NA,
                 MoA = NA)
)

# Default inits ----
init_defaults <- list(
  Lemna_Schmitt = list(BM = 0.0012, E = 1, M_int = 0),
  Myrio = list(BM = 1, M_int = 0),
  Myrio_log = list(BM = 1, M_int = 0),
  Algae_Weber = list(A = 1, Q = 0.01, P = 0.18, C = 0),
  Lemna_SETAC = list(BM = 0.0012, M_int = 0),
  Algae_Simple = list(A = 1, Dw = 0)
)

# Default forcings ----
forcing_defaults <- list(
  Lemna_Schmitt = list(
    temp = structure(list(t = 0, value = 12), class = "data.frame", row.names = c(NA,-1L)), 
    rad = structure(list(t = 0, value = 15000), class = "data.frame", row.names = c(NA,-1L))
  ),
  Algae_Weber = list(
    T_act = structure(list(t = 0, value = 12), class = "data.frame", row.names = c(NA,-1L)),
    I = structure(list(t = 0, value = 76), class = "data.frame", row.names = c(NA,-1L)),
    C_in = structure(list(t = 0, value = 1), class = "data.frame", row.names = c(NA,-1L))
  ),
  Lemna_SETAC = list(
    tmp = structure(list(t = 0, value = 12), class = "data.frame", row.names = c(NA,-1L)),
    irr = structure(list(t = 0, value = 15000), class = "data.frame", row.names = c(NA,-1L)),
    P = structure(list(t = 0, value = 0.3), class = "data.frame", row.names = c(NA,-1L)), 
    N = structure(list(t = 0, value = 0.6), class = "data.frame", row.names = c(NA,-1L))
  ),
  Algae_Simple = list(    
    f_growth = structure(list(t = 0, value = 1), class = "data.frame", row.names = c(NA,-1L))
  )
)

# Set model defaults ----
model_defaults <- list(
  Lemna_Schmitt = list(
    init_defaults = init_defaults[["Lemna_Schmitt"]],
    parameter_defaults = parameter_defaults[["Lemna_Schmitt"]],
    forcing_defaults = forcing_defaults[["Lemna_Schmitt"]]
  ),
  Myrio = list(    
    init_defaults = init_defaults[["Myrio"]],
    parameter_defaults = parameter_defaults[["Myrio"]]
  ),
  Myrio_log = list(    
    init_defaults = init_defaults[["Myrio_log"]],
    parameter_defaults = parameter_defaults[["Myrio_log"]]
  ), 
  Algae_Weber = list(  
    init_defaults = init_defaults[["Algae_Weber"]],
    parameter_defaults = parameter_defaults[["Algae_Weber"]],
    forcing_defaults = forcing_defaults[["Algae_Weber"]]
  ),
  Lemna_SETAC = list(
    init_defaults = init_defaults[["Lemna_SETAC"]],
    parameter_defaults = parameter_defaults[["Lemna_SETAC"]],
    forcing_defaults = forcing_defaults[["Lemna_SETAC"]]
  ),
  Algae_Simple = list(    
    init_defaults = init_defaults[["Algae_Simple"]],
    parameter_defaults = parameter_defaults[["Algae_Simple"]],
    forcing_defaults = forcing_defaults[["Algae_Simple"]]
  )
)

usethis::use_data(model_defaults, overwrite = TRUE)
