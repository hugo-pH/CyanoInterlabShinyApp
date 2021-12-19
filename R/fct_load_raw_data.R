#' helpers
#'
#' @param path 
#' @param pattern 
#'
#' @description Function to read a single experimental run excel file
#'
#' @return a nested data.frame containing all the sheets from the excel file
#'
#' @noRd
read_experiment_file <- function(path, pattern) {
  # extract experiment date from path
  experiment.date <- sub(pattern, "\\2", path)

  # extract file name without extension from file path
  experiment.id <- stringr::str_split(path, "/") %>%
    purrr::flatten_chr() %>%
    dplyr::last() %>%
    stringr::str_split("\\.") %>%
    purrr::flatten_chr() %>%
    dplyr::first()

  # read all sheets from excel
  result <- path %>%
    readxl::excel_sheets() %>%
    rlang::set_names() %>%
    purrr::map(readxl::read_excel, path = path) %>%
    tibble::enframe() %>%
    dplyr::mutate(
      experiment_date = experiment.date,
      experiment_id = experiment.id
    )


  return(result)
}


#' @description Generate a single table with all the experiments
#'
#' @return a nested data.frame containing all the sheets from the excel file
#'
#' @noRd

load_experiments <- function(path, pattern) {
  # browser()
  data.files <- list.files(
    path, pattern,
    full.names = TRUE
  )

  results <- purrr::map_dfr(
    data.files, read_experiment_file,
    pattern = pattern
  )

  return(results)
}
