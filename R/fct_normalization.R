#' Correct OD or fluorescence with data from blank wells
#'
#'@description Function to extract the background measurements from each plate and subtract it from the samples' measurements.
#'
#' @param df A plate reader data.frame.
#' @param .var  The unquoted variable to be background-corrected.
#' @param column_suffix The suffix that will be appended to the column name of the background-corrected variable.
#'
#' @description function  to extract the background measurements from each plate and subtract it from the actual measurements
#'
#' @return The same data.frame as the input but with an extra column with the background corrected variable
#'
#' @examples 
#' background_correction(df = df.od, .var = OD_730, column_suffix = "bc")
#' 
background_correction <- function(df, .var, column_suffix) {
  var <- rlang::enquo(.var)
  blank_var_nm <- paste0("blank_", rlang::as_label(var))
  var_corrected_nm <- paste(rlang::as_label(var), column_suffix, sep = "_")

  # get the mean background value for each timepoint and experiment
  df.blank <- df %>%
    dplyr::filter(!is.na({{ .var }}), sample_type == "blank") %>%
    dplyr::group_by(location, bio_replicate, time_h) %>%
    dplyr::summarise(
      !!blank_var_nm := mean({{ .var }})
    ) %>%
    dplyr::ungroup()

  # join the summarized background values to the measurements and subtract it from them
  df %>%
    dplyr::filter(!is.na({{ .var }}), sample_type == "sample") %>%
    dplyr::left_join(df.blank, by = c("location", "bio_replicate", "time_h")) %>%
    dplyr::mutate(
      # subtract blank
      !!var_corrected_nm := ifelse(time_h == 24,
        # if timepoint is 24h, subtract 5 times the blank
        {{ .var }} - (.data[[blank_var_nm]] * 5),
        {{ .var }} - .data[[blank_var_nm]]
      )
    )

}

#' Normalize plate reader fluorescence values with OD
#'
#' @description  Function to normalize fluorescence values by corresponding OD values in plate reader data.
#'
#' @param df.od The plate reader OD data.frame.
#' @param df.fl The plate reader fluorescence data.frame.
#' @param od_col The unqouted column name containing the OD values.
#' @param fl_col The unqouted column name containing the fluorescence values.
#' @param bc_suffix The suffix that will be appended to the column name of the background-corrected variable when \code{background_correction} = T
#' @param background_correction Logical indicating if background correction should be performed.
#'
#' @return A data.frame containing a new column with fluorescence per OD values.
#'
#' @examples
od_correction <- function(df.od, df.fl, od_col = OD_730, fl_col = fl, bc_suffix = "bc", background_correction = T){
  
  
  if (background_correction) {
    df.od <-  background_correction(df = df.od, .var = {{od_col}}, column_suffix = bc_suffix)
    df.fl <-  background_correction(df = df.fl, .var = {{fl_col}}, column_suffix = bc_suffix)
    
    od_col <- stringr::str_subset(colnames(df.od), bc_suffix) %>% as.name()
    fl_col <- stringr::str_subset(colnames(df.fl), bc_suffix) %>% as.name()
  }
  
  
  
  dplyr::left_join(
    df.od,
    df.fl,
    by = c("strain", "induction", "bio_replicate", "location", "tech_replicate", "experiment_date", "experiment_id", "time_h", "sample_type")
  ) %>%
    dplyr::mutate(
      fl_od = {{fl_col}} / {{od_col}}
    )
}



#' Normalize plate reader values using a given strain and induction condition
#' 
#' @description  Function to normalize by a strain and induction.
#'
#' @param df A plate reader data.frame containing both OD and fluorescence values.
#' @param .var The unquoted column name of the variable to be normalized (i.e. fl_od).
#' @param ref_strain The reference strain to normalize with.
#' @param ref_induction The reference induction condition to normalize with.
#' @param column_suffix The suffix that will be appended to the column name of the normalized variable.
#'
#' @return A data.frame containing a new column with normalized variable.
#'
#' @examples
#' strain_normalization(df.pr.bc, 
#' .var = fl_od, ref_strain = "J23100", 
#' ref_induction = "-", 
#' column_suffix = "norm")
#' 
#' 
strain_normalization <- function(df, .var, ref_strain, ref_induction, column_suffix){
  # browser()
  var <- rlang::enquo(.var)
  strain_var_nm <- paste(ref_strain, rlang::as_label(var), sep = "_")
  var_corrected_nm <- paste(rlang::as_label(var), column_suffix, sep = "_")
  
  # get the mean value of the reference strain for each timepoint and experiment
  df.ref.strain <- df %>%
    dplyr::filter(!is.na({{.var}}), strain == ref_strain, induction == ref_induction) %>%
    dplyr::group_by(location, bio_replicate, time_h) %>%
    dplyr::summarise(
      !!strain_var_nm := mean({{.var}})
    ) %>%
    dplyr::ungroup()
  
  # join the summarized reference values to the measurements and normalize
  df %>%
    dplyr::filter(!is.na({{.var}}), sample_type == "sample") %>%
    dplyr::left_join(df.ref.strain, by = c("location", "bio_replicate", "time_h")) %>%
    dplyr::mutate(
      # normalize by reference strain
      !!var_corrected_nm := {{.var}} / .data[[strain_var_nm]]
    )
  
}

#' Run plate reader normalization pipeline 
#' 
#' @description  Function to combine all the background correction and normalization steps
#'
#' @param df.od The plate reader OD data.frame.
#' @param df.fl The plate reader fluorescence data.frame.
#' @param bc_suffix The suffix that will be appended to the column name of the background-corrected variable.
#' @param od_col The unqouted column name containing the OD values.
#' @param fl_col The unqouted column name containing the fluorescence values.
#' @param background_correction Logical indicating if background correction should be performed.
#' @param .norm_var The unquoted column name of the variable to be normalized (i.e. fl_od).
#' @param ref_strain The reference strain to normalize with.
#' @param ref_induction The reference induction condition to normalize with.
#' @param norm_suffix The suffix that will be appended to the column name of the normalized variable.
#'
#' @return A single data.frame containing new columns for background-corrected, fluorescence per OD and reference strain normalized data.
#'
#' @examples df.plate.reader <- plate_reader_normalization_pipeline(
#' df.od = load_plate_reader_od(df.experiments.raw),
#' df.fl = load_plate_reader_fl(df.experiments.raw),
#' bc_suffix = "bc",
#' .norm_var = fl_od,
#' ref_strain = "J23100",
#' ref_induction = "-",
#' norm_suffix  = "norm")
#' 
#'   
plate_reader_normalization_pipeline <- function(df.od, df.fl, bc_suffix,
                                                od_col = OD_730, fl_col = fl,
                                                background_correction = T,
                                                .norm_var,
                                                ref_strain, ref_induction,
                                                norm_suffix){
  # perform OD correction
  df.pr.bc <- od_correction(
    df.od = df.od,
    df.fl = df.fl,
    background_correction = background_correction,
    bc_suffix = bc_suffix
  )
  # normalized by reference strain
  strain_normalization(df.pr.bc,
                                  .var = fl_od,
                                  ref_strain = ref_strain,
                                  ref_induction = ref_induction,
                                  column_suffix = norm_suffix)
  
}


## Full spectrum -------------------------------------------
# normalize full spectrum measurementsf from 0 to 1 for each location, run, timepoint, strain and condition
normalize_full_spectrum <- function(df){
  
  df %>%
    dplyr::group_by(location, experiment_date, strain, induction, time_h) %>%
    dplyr::mutate(
      abs_norm = (abs  - min(abs)) / (max(abs) - min(abs))
    )
}



