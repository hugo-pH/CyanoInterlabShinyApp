## code to prepare `data` dataset goes here

df.experiments.raw <- load_experiments(here::here("data-raw"),
  pattern = "(.*/)?([0-9]{8})_([[:alpha:]]+)_([[:alnum:]]+).xlsx"
)

# Spectrophotometer OD 730 -------------------------------------

df.spectrophotometer <-
  load_spectrophotometer(df.experiments.raw) %>%
  # remove runs without spectrophotometer OD measurements
  dplyr::filter(!is.na(location))


# Full spectrum ------------------------------------------------

df.full.spectrum <- load_full_spectrum(df.experiments.raw) %>% 
  # remove runs without full spectrum OD measurements
  dplyr::filter(!is.na(location), !is.na(abs)) %>% 
  normalize_full_spectrum()



# Plate reader -------------------------------------------------

df.plate.reader <- plate_reader_normalization_pipeline(
  df.od = load_plate_reader_od(df.experiments.raw),
  df.fl = load_plate_reader_fl(df.experiments.raw),
  bc_suffix = "bc",
  .norm_var = fl_od,
  ref_strain = "J23100",
  ref_induction = "-",
  norm_suffix  = "norm"
)



usethis::use_data(df.spectrophotometer, df.full.spectrum, df.plate.reader, overwrite = TRUE)
