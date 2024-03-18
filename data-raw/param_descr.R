# Parameter descriptions ----
filepath <- paste0(here::here(), "/inst/extdata/", "parameter_descriptions.csv")
parameter_descriptions <- read.table(filepath, sep = ";", header = TRUE)

usethis::use_data(parameter_descriptions, overwrite = TRUE)


# Model descriptions ----
model_descriptions <- list(
  Lemna_Schmitt = "The model is a mechanistic combined toxicokinetic-toxicodynamic (TK/TD) and growth model for the aquatic macrophytes Lemna spp. The model simulates the development of Lemna biomass under laboratory and environmental conditions and was developed by Schmitt et al. (2013). Growth of the Lemna population is simulated on basis of photosynthesis and respiration rates which are functions of environmental conditions. The toxicodynamic sub-model describes the effects of growth-inhibiting substances by a respective reduction in the photosynthesis rate based on internal concentrations.",
  Lemna_SETAC = "TBD. Lemna model by Klein et al. (2021).",
  Myrio = "Model is derived from the Lemna TKTD model by Klein et al. (2021).",
  Myrio_log = "Model is derived from the Lemna TKTD model by Klein et al. (2021).",
  Algae_Weber = "TBD. Model is derived from the Algae model by Weber (2012) as cited in EFSA TKTD opinion (2018)"
)
usethis::use_data(model_descriptions, overwrite = TRUE)
