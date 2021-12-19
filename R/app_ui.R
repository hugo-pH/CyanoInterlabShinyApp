#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  navbarPage(
    id = "navbarid",
    "Interlab study",
    theme = shinythemes::shinytheme("sandstone"),
    tabPanel("Intro",
             value = "intro",
             includeMarkdown("./md/intro.md"),
             hr()),
    tabPanel("Spectrophotometer OD",
             value = "sp_od",
             mod_spectrophotometer_ui("sp", df = df.spectrophotometer)
            ),
    tabPanel("Plate Reader",
             value = "pr",
             mod_plate_reader_ui("pr", df = df.plate.reader)
             ),
    tabPanel("Full spectrum",
             value = "fs",
             mod_full_spectrum_ui("fs", df = df.full.spectrum)
    )
  )
 
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CyanoInterlabShinyApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
