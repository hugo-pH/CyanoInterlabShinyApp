#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
 
  
  # spectrophotometer tab
  mod_sp_growth_curve_server("sp", df = df.spectrophotometer)
  mod_sp_growth_rate_server("sp_gr", df = df.spectrophotometer)
  # plate reader tab
  mod_select_pr_trans_server("pr")
  mod_plate_reader_server("pr", df = df.plate.reader)
  
  # full spectrum tab
  mod_full_spectrum_server("fs", df = df.full.spectrum)
}
