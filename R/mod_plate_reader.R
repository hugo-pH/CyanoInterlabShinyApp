#' plate_reader UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plate_reader_ui <- function(id, df) {
  ns <- NS(id)
  fluidRow(
    sidebarPanel(
      width = 3,
      location_replicates_ui("pr", df = df),
      plot_colours_ui("pr"),
      plot_columns_ui("pr"),
      plot_rows_ui("pr"),
      select_var_ui("pr"),
      select_pr_trans_ui("pr"),
      free_y_axis_ui("pr")
      # plot_log_ui("pr")
      
    ),
    mainPanel(plot_ui("pr"))
    
  )
}

select_var_ui <- function(id) {
  tagList(selectInput(
    NS(id, "pr_var"),
    "Select variable to be shown:",
    choices =  c(
      "OD" = "OD_730",
      "Fluorescence" = "fl",
      "Fluorescence per OD" = "fl_od"
    )
  ))
}

select_pr_trans_ui <- function(id) {
  uiOutput(NS(id, "select_pr_trans"))
}



#' spectrophotometer Server Functions
#'
#' @noRd

mod_select_pr_trans_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    var <- reactive(input$pr_var)
    
    output$select_pr_trans <- renderUI({
      value <- isolate(input$pr_trans)
      
      if (var() %in% c("OD_730", "fl")) {
        selectInput(
          NS(id, "pr_trans"),
          "Select the data transformation",
          choices = c("Raw" = "raw", "Background-corrected" = "bc")
        )
      } else {
        selectInput(
          NS(id, "pr_trans"),
          "Select the data transformation",
          choices = c(
            "Background-corrected" = "bc",
            "J23100-normalized" = "norm"
          )
        )
      }
    })
    
  })
}




mod_plate_reader_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    data <- reactive(
      df %>%
        dplyr::filter(
          location %in% input$location,
          bio_replicate %in% input$bio_rep
        )
    )
    
    colour_var <- reactive(input$plot_colour)
    columns_var <- reactive(input$plot_columns)
    rows_var <- reactive(input$plot_rows)
    log_plot <- reactive(input$log_plot)
    y_var <- reactive(input$pr_var)
    y_trans <- reactive(input$pr_trans)
    free_y_scales <- reactive(input$free_y_axis)
    
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
        validate("Please, select different for colour and the plot grid.")
      }
      
      if ((y_trans() == "raw" & y_var() == "fl_od") |
          (y_trans() == "norm" & y_var() %in% c("OD_730", "fl"))) {
        validate("The combination of variable and transformation is not valid.")
      }
      
      # browser()
      if (y_var() %in% c("OD_730", "fl")) {
        if (y_trans() == "raw") {
          y_var_trans <- y_var()
        } else{
          y_var_trans <- stringr::str_c(y_var(), y_trans(), sep = "_")
        }
      } else{
        if (y_trans() == "bc") {
          y_var_trans <- y_var()
        } else{
          y_var_trans <- stringr::str_c(y_var(), y_trans(), sep = "_")
        }
      }
      
      plot_summary(
        data(),
        y_var = y_var_trans,
        colour_var = colour_var(),
        f_rows_var = rows_var(),
        f_cols_var = columns_var(),
        df.labels = df.var.labels,
        log_space = F,
        free_y_scale = free_y_scales()
      )
      
    })
    
  })
}
