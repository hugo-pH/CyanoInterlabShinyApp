

load_spectrophotometer <- function(df) {
  
  df %>%
    dplyr::filter(name == "OD 730 nm Spectrophotometer") %>%
    tidyr::unnest(value) %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(cols = starts_with("T"),
                 values_to = "OD_730",
                 names_to = "timepoint") %>%
    dplyr::mutate(time = as.numeric(stringr::str_remove_all(timepoint, "[[:alpha:]]")),
           time_h = ifelse(time == 24,
                           time,
                           time / 60)) %>%
    tidyr::separate(sample,
             into = c("strain", "induction"),
             sep = " ") %>%
    dplyr::select(-c(name, timepoint, time))
  
}



#' load_datasets
#'
#' @description Function to extract the plate reader OD measurements from the master dataset (data) containing all data sources
#'
#' @return A data.frame with the plate reader OD measurements
#'
#' @noRd
load_plate_reader_od <- function(df) {
  
  df %>%
    dplyr::filter(name == "OD Plate Reader") %>%
    tidyr::unnest(value) %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(
      cols = t0:t24h,
      values_to = "OD_730",
      names_to = "timepoint"
    ) %>%
    dplyr::mutate(
      time = as.numeric(stringr::str_remove_all(timepoint, "[[:alpha:]]")),
      time_h = ifelse(time == 24,
        time,
        time / 60
      )
    ) %>%
    tidyr::separate(sample,
      into = c("strain", "induction"),
      sep = " "
    ) %>%
    dplyr::mutate(
      sample_type = ifelse(strain == "Blank", "blank", "sample"),
      strain = ifelse(strain == "Blank", NA, strain)
    ) %>%
    dplyr::select(-c(name, timepoint, time))
  
}



load_plate_reader_fl <- function(df) {
  
  df %>%
    dplyr::filter(name == "Fluorescence Plate Reader") %>%
    tidyr::unnest(value) %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(cols = t0:t24h,
                 values_to = "fl",
                 names_to = "timepoint") %>%
    dplyr::mutate(time = as.numeric(stringr::str_remove_all(timepoint, "[[:alpha:]]")),
           time_h = ifelse(time == 24,
                           time,
                           time / 60)) %>%
    tidyr::separate(sample,
             into = c("strain", "induction"),
             sep = " ") %>%
    dplyr::mutate(sample_type = ifelse(strain == "Blank", "blank", "sample")) %>%
    dplyr::select(-c(name, timepoint, time))
  
}


load_full_spectrum <- function(df) {
  # browser()
  df %>%
    dplyr::filter(name == "Spectrum Spectrophotometer") %>%
    tidyr::unnest(value) %>%
    # clean_names() %>%
    tidyr::pivot_longer(
      cols = EVC_0:`petE_24h +`,
      values_to = "abs",
      names_to = "id"
    ) %>%
    tidyr::separate(id, into = c("strain", "id"), sep = "_") %>%
    tidyr::separate(id,
             into = c("timepoint", "induction"),
             sep = " ") %>%
    dplyr::mutate(time = as.numeric(stringr::str_remove_all(timepoint, "[[:alpha:]]")),
           time_h = ifelse(time == 24,
                           time,
                           time / 60)) %>%
    tidyr::separate(
      experiment_id,
      into = c("dummy1", "location", "dummy2"),
      sep = "_",
      remove = F
    ) %>%
    dplyr::select(-c(name, timepoint, time, dummy1, dummy2)) %>%
    # create a column with the biological replicate
    dplyr::group_by(location) %>%
    dplyr::arrange(experiment_date) %>%
    dplyr::mutate(
      bio_replicate = dplyr::ntile(n = length(unique(experiment_date)))
    )
  
  
}
