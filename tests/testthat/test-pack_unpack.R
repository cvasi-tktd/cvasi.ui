

test_that("pack/unpack works", {
  dat_out <- list(
    active_model = "Lemna_Schmitt", 
    param = list(Emax = 1, AperBM = 1000, 
                 Kbm = 1, P_Temp = 0, MolWeight = 381, k_phot_fix = 1, k_phot_max = 0.42, 
                 k_resp = 0, k_loss = 0, Tmin = 8, Tmax = 40.5, Topt = 26.7, 
                 t_ref = 25, Q10 = 2, k_0 = 3, a_k = 5e-05, C_P = 0.3, CP50 = 0.0043, 
                 a_P = 1, KiP = 101, C_N = 0.6, CN50 = 0.034, a_N = 1, KiN = 604, 
                 BM50 = 176, mass_per_frond = 1e-04, BMw2BMd = 16.7, EC50 = 0.3, 
                 b = 4.16, P_up = 0.0054), 
    init = list(BM = 0.0012, E = 1, M_int = 0)
  )
  zipfile <- paste0(tempdir(), "/test.zip")
  out_path <- pack(dat_out, zipfile)
  imported <- unpack(zipfile)
  
  expect_true(file.exists(zipfile))
  expect_equal(zipfile, out_path)
  expect_equal(dat_out, imported)
})
