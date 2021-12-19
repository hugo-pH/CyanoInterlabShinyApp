#' spectrophotometer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spectrophotometer_ui <- function(id, df){
  ns <- NS(id)
  fluidRow(
    sidebarPanel(width = 3,
                 h3("Define growth curve plot"),
                 location_replicates_ui("sp", df = df),
                 plot_colours_ui("sp"),
                 plot_columns_ui("sp"),
                 plot_rows_ui("sp"),
                 plot_log_ui("sp"),
                 hr(),
                h3("Define growth rate plot"),
                location_replicates_ui("sp_gr", df = df),
                x_axis_growth_plot_ui("sp_gr"),
                plot_colours_ui("sp_gr"),
                plot_columns_ui("sp_gr"),
                plot_rows_ui("sp_gr")
    ),
    mainPanel(
      plot_ui("sp"),
      plot_growth_ui("sp_gr")
      )
    
  )
}


x_axis_growth_plot_ui <- function(id){
  tagList(
    selectInput(NS(id, "growth_plot_x"), "Select variable to show in X-axis for the growth rates plot:", 
                choices = c("Location" = "location", "Induction regime" = "induction", "Strain" = "strain"),
                selected = "strain")
  )
}

#' spectrophotometer Server Functions
#'
#' @noRd 
mod_sp_growth_curve_server <- function(id, df){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data <- reactive(
      df %>%
        dplyr::filter(location %in% input$location, bio_replicate %in% input$bio_rep)
    )
    
    colour_var <- reactive(input$plot_colour)
    columns_var <- reactive(input$plot_columns)
    rows_var <- reactive(input$plot_rows)
    log_plot <- reactive(input$log_plot)
    
    
    output$plot <- renderPlot({
      req(input$location, input$bio_rep, input$plot_colour, input$plot_columns, input$plot_rows)
      # browser()
      if (columns_var() == rows_var()) {
        validate("Please, select different variables for rows and columns")
      }
      
      if (colour_var() == rows_var() |
          colour_var() == columns_var()) {
        validate("Please, select different for colour and the plot grid")
      }
      plot_summary(data(), y_var = "OD_730",
                   colour_var = colour_var(),
                   f_rows_var = rows_var(),
                   f_cols_var = columns_var(),
                   df.labels = df.var.labels,
                   log_space = log_plot()
                   )
      
    })
    
  })
}

mod_sp_growth_rate_server <- function(id, df){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    df.growth <- df %>% 
    dplyr::filter(!is.na(location)) %>% 
      dplyr::group_by(location, bio_replicate, strain, induction) %>% 
      tidyr::nest() %>% 
      dplyr::mutate(
        model = purrr::map(data, ~lm(log(OD_730)~time_h, data = .)),
        coeffs = purrr::map(model, broom::tidy),
        r_squared = purrr::map(model, broom::glance)
      ) %>% 
      tidyr::unnest(coeffs) %>% 
      dplyr::filter(term == "time_h") %>% 
      dplyr::select(-c(std.error, statistic, p.value)) %>% 
      tidyr::unnest(r_squared) 
    
    
    data <- reactive(
      df.growth %>%
        dplyr::filter(location %in% input$location, bio_replicate %in% input$bio_rep)
    )
    x_var <- reactive(input$growth_plot_x)
    colour_var <- reactive(input$plot_colour)
    columns_var <- reactive(input$plot_columns)
    rows_var <- reactive(input$plot_rows)
    log_plot <- reactive(input$log_plot)
    
    
    output$plot_growth <- renderPlot({
      req(input$location, input$bio_rep, input$plot_colour, input$plot_columns, input$plot_rows)
      # browser()
      if (columns_var() == rows_var()) {
        validate("Please, select different variables for rows and columns.")
      }
      
      if (colour_var() == rows_var() |
          colour_var() == columns_var()) {
        validate("Please, select different variables for colour and the plot grid.")
      }
      
      if(x_var() == rows_var()){
        validate("Please, select different variables for X-axis and the plot grid rows.")
      }
      
      plot_growth_rates(data(), x_var = x_var(),
                        
                   colour_var = colour_var(),
                   f_rows_var = rows_var(),
                   f_cols_var = columns_var()
      )
      
    })
    
  })
}
    