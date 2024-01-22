
# Model descriptions ----
model_descriptions <- list(
  Lemna_Schmitt = "The model is a mechanistic combined toxicokinetic-toxicodynamic (TK/TD) and growth model for the aquatic macrophytes Lemna spp. The model simulates the development of Lemna biomass under laboratory and environmental conditions and was developed by Schmitt et al. (2013). Growth of the Lemna population is simulated on basis of photosynthesis and respiration rates which are functions of environmental conditions. The toxicodynamic sub-model describes the effects of growth-inhibiting substances by a respective reduction in the photosynthesis rate based on internal concentrations.",
  Lemna_SETAC = "TBD. Lemna model by Klein et al. (2021).",
  Myrio = "Model is derived from the Lemna TKTD model by Klein et al. (2021).",
  Myrio_log = "Model is derived from the Lemna TKTD model by Klein et al. (2021).",
  Algae_Weber = "TBD. Model is derived from the Algae model by Weber (2012) as cited in EFSA TKTD opinion (2018)"
)


# Model choices ----
model_choices <- list("Lemna_Schmitt", "Myrio", "Myrio_log", "Algae_Weber")


# Default parameters ----
parameter_defaults <- list(
  GUTS_RED_SD = list(kd = 22, hb = 0.01, z = 0.5, kk = 0.08), # From howto
  GUTS_RED_IT = list(kd=1.2, hb=0, alpha=9.2, beta=4.3), # From Manual
  Lemna_Schmitt = list(Emax = 0.784, AperBM = 1000, Kbm = 0.75, P_Temp = FALSE, 
                       MolWeight = 381, k_phot_fix = FALSE, k_phot_max = 0.47, k_resp = 0.05, 
                       k_loss = 0, Tmin = 8, Tmax = 40.5, Topt = 26.7, t_ref = 25, 
                       Q10 = 2, k_0 = 3, a_k = 5e-05, C_P = 0.3, CP50 = 0.0043, 
                       a_P = 1, KiP = 101, C_N = 0.6, CN50 = 0.034, a_N = 1, KiN = 604, 
                       BM50 = 176, mass_per_frond = 1e-04, BMw2BMd = 16.7, EC50 = 0.3, 
                       b = 4.16, P_up = 0.0054), # from metsulfuron()@param
  Lemna_SETAC = list(k_photo_fixed = 0, k_photo_max = 0.47, k_loss = 0.05, BM_min = 0, 
                     T_opt = 26.7, T_min = 8, T_max = 40.5, Q10 = 2, T_ref = 25, 
                     alpha = 5e-05, beta = 0.025, N_50 = 0.034, P_50 = 0.0043, 
                     BM_L = 176, E_max = 0.784, r_A_DW = 1000, r_FW_DW = 16.7, 
                     r_FW_V = 1, r_DW_FN = 4e-04, K_pw = 0.75, k_met = 0, EC50_int = 0.3, 
                     b = 4.16, P = 0.0054),
  Myrio = list(k_photo_max = 0.47, E_max = 1, r_A_DW = 1000, r_FW_DW = 16.7, r_FW_V = 1, K_pw = 1, k_met = 0, # from Myrio()@param
               EC50_int = 1, b = 1, P = 0.0001, r_DW_TSL = 0.0001 # 
               ), 
  Myrio_log = list(k_photo_max = 0.47, E_max = 1, r_A_DW = 1000, r_FW_DW = 16.7, r_FW_V = 1, K_pw = 1, k_met = 0, # from Myrio_log()@param
                   EC50_int = 1, b = 1, P = 0.0001, r_DW_TSL = 0.0001, BM_L = 1000 # 
                   ),
  Algae_Weber = list(mue_max=1.738, m_max=0.05, v_max=0.052, k_s=0.068, Q_min=0.0011, Q_max=0.0144, R_0=0.36, D=0.5, T_opt=27, T_min=0, T_max=35, I_opt=120, EC_50=115, b=1.268, k=0.2),
  DEB_abj = list(p_M = NA, v = NA, k_J = NA,
                 p_Am = NA, kap = NA, E_G = NA,
                 f = NA, E_Hj = NA, E_Hp = NA, 
                 kap_R = NA, ke = NA, c0 = NA,
                 cT = NA, L_b = NA, L_j = NA,
                 MoA = NA)
)


# Default exposure ----
default_exposure <- structure(list(time = 0:21, 
                                   conc = c(0.0446366030420265, 0.0182426852446832, 
                                            0.00700628396314715, 0.0562743836226876, 0.107869605187729, 0.0469605931902834, 
                                            0.0193346152523222, 0.0609656879124701, 0.0554827123530596, 0.163047160105389, 
                                            0.127683276153348, 0.158718330767003, 0.026904620494612, 0.143767619460672, 
                                            0.100365184698185, 0.156334785861355, 0.113797203207325, 0.146109535594881, 
                                            0.110069238021521, 0.273273198953883, 0.275168041826428, 0.277150801656334
                                   ), trial = c("T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", 
                                                "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", 
                                                "T1", "T1", "T1")), row.names = c(NA, -22L), class = "data.frame")


# Default forcings ----
forcing_defaults <- list(
  temp = structure(list(t = 0, value = 12), class = "data.frame", row.names = c(NA,-1L)), 
  rad = structure(list(t = 0, value = 15000), class = "data.frame", row.names = c(NA,-1L)),
  tmp = structure(list(t = 0, value = 12), class = "data.frame", row.names = c(NA,-1L)),
  irr = structure(list(t = 0, value = 15000), class = "data.frame", row.names = c(NA,-1L)),
  P = structure(list(t = 0, value = 0.3), class = "data.frame", row.names = c(NA,-1L)), 
  N = structure(list(t = 0, value = 0.6), class = "data.frame", row.names = c(NA,-1L)),
  T_act = structure(list(t = 0, value = 12), class = "data.frame", row.names = c(NA,-1L)),
  I = structure(list(t = 0, value = 76), class = "data.frame", row.names = c(NA,-1L)),
  C_in = structure(list(t = 0, value = 1), class = "data.frame", row.names = c(NA,-1L))
)

