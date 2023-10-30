



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
  Lemna_SETAC = list(k_photo_fixed = 0, k_photo_max = 0.47, k_loss = 0.05,
                     BM_min = 5e-04, T_opt = 26.7, T_min = 8,
                     T_max = 40.5, Q10 = 2, T_ref = 25,
                     alpha = 5e-05, beta = 0.025, N_50 = 0.034,
                     P_50 = 0.0043, BM_L = 177, E_max = 1, 
                     r_A_DW = 1000, r_FW_DW = 16.7, r_FW_V = 1,
                     r_DW_FN = 1e-04, K_pw = 1, k_met = 0), # from Lemna_SETAC()@param
  Myrio = list(k_photo_max = 0.47, E_max = 1, r_A_DW = 1000, r_FW_DW = 16.7, 
               r_FW_V = 1, K_pw = 1, k_met = 0), # from Myrio()@param
  DEB_abj = list(p_M = NA, v = NA, k_J = NA,
                 p_Am = NA, kap = NA, E_G = NA,
                 f = NA, E_Hj = NA, E_Hp = NA, 
                 kap_R = NA, ke = NA, c0 = NA,
                 cT = NA, L_b = NA, L_j = NA,
                 MoA = NA)
)

model_choices <- c("GUTS_RED_SD", "GUTS_RED_IT", "Lemna_Schmitt", "Lemna_SETAC", "Myrio", "DEB_abj")
#model_choices <- names(parameter_defaults)





# 
# lapply(setNames(model_choices,model_choices), function(m){
#   f <- get(m)
#   x <- f()
#   list(param.req = x@param.req)
# })
# 
# 
# 
# 
# a <- numeric()
# a
# ex@param.req
# ?numeric
# 
# Lemna_Schmitt()@param.req
# dput(do.call(c, Lemna_Schmitt()@param))
