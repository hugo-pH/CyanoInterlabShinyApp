#' full_spectrum UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_full_spectrum_ui <- function(id, df) {
  ns <- NS(id)
  fluidRow(
    sidebarPanel(
      width = 3,
      location_replicates_ui("fs", df = df),
      plot_colours_ui("fs"),
      plot_columns_ui("fs"),
      plot_rows_ui("fs"),
      select_fs_trans_ui("fs")
      
    ),
    mainPanel(plot_ui("fs"))
    
  )
}


select_fs_trans_ui <- function(id) {
  selectInput(
    NS(id, "fs_trans"),
    "Select the data transformation",
    choices = c("Raw" = "raw", "Normalized" = "norm"), selected = "norm"
  )
}


mod_full_spectrum_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    
    # time 0 doesn't have induction values because is the same measurement for both conditions
    # so we label it as both + and - induction for plotting purposes
    df.plot <- dplyr::bind_rows(
      df %>% 
        dplyr::filter(time_h == 0) %>% 
        dplyr::mutate(
          induction = "+"
        ),
      df %>% 
        dplyr::filter(time_h == 0) %>% 
        dplyr::mutate(
          induction = "-"
        ) 
    ) %>% 
      dplyr::bind_rows(
        df %>% 
          dplyr::filter(time_h != 0)    
      )
    
    data <- reactive(
     df.plot %>%
        dplyr::filter(
          location %in% input$location,
          bio_replicate %in% input$bio_rep
        )
     )
    
    colour_var <- reactive(input$plot_colour)
    columns_var <- reactive(input$plot_columns)
    rows_var <- reactive(input$plot_rows)
    y_trans <- reactive(input$fs_trans)
    
    output$plot <- renderPlot({
      req(
        input$location,
        input$bio_rep,
        input$plot_colour,
        input$plot_columns,
        input$plot_rows
      )
      # browser()
      # if (input$plot_columns == input$plot_rows) {
      if (columns_var() == rows_var()) {
        validate("Please, select different variables for rows and columns.")
      }
      
      if (colour_var() == rows_var() |
          colour_var() == columns_var()) {
        validate("Please, select different variables for colour and the plot grid.")
      }
         # browser()
        if (y_trans() == "raw") {
          y_var_trans <- "abs"
        } else{
          y_var_trans <- stringr::str_c("abs", y_trans(), sep = "_")
        }
      
      
      plot_full_spectrum(
        data(),
        y_var = y_var_trans,
        colour_var = colour_var(),
        f_rows_var = rows_var(),
        f_cols_var = columns_var()
        )
      
    })
    
  })
}
