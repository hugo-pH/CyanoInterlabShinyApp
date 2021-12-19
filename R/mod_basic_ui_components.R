#' ui_components UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
location_replicates_ui <- function(id, df) {
  tagList(
    selectInput(NS(id, "location"), "Location", 
                choices = unique(df$location), 
                multiple = TRUE,
                selected = unique(df$location)),
    selectInput(NS(id, "bio_rep"), "Biological Replicate", multiple = TRUE,
                choices = c(as.character(unique(df$bio_replicate))),
                selected = c(as.character(unique(df$bio_replicate)))
                )
    
  )
}


plot_colours_ui <- function(id){
  tagList(
    selectInput(NS(id, "plot_colour"), "Colour datapoints by:",
                choices = c("Location" = "location", "Induction regime" = "induction", "Strain" = "strain"),
                selected = "strain")
  )
}

plot_columns_ui <- function(id){
  tagList(
    selectInput(NS(id, "plot_columns"), 
                "Select the columns for the plot grid",choices =  c("Location" = "location", "Induction regime" = "induction", "Strain" = "strain"),
                selected = "location")
  )
}

plot_rows_ui <- function(id){
  tagList(
    selectInput(NS(id, "plot_rows"), "Select the rows for the plot grid", 
                choices = c("Location" = "location", "Induction regime" = "induction", "Strain" = "strain"),
                selected = "induction")
  )
}



var_to_plot_ui <- function(id, var_choices){
  tagList(
    radioButtons(NS(id, "y_var"), "Select variable to plot:",
                 var_choices
    )
  )
}

plot_log_ui <- function(id, var_choices){
  tagList(
    checkboxInput(NS(id, "log_plot"), label = "Use log transformation in growth curves", value = FALSE
    )
  )
}

free_y_axis_ui <- function(id, var_choices){
  tagList(
    checkboxInput(NS(id, "free_y_axis"), label = "Adjust Y-axis scale per row", value = FALSE
    )
  )
}

## UI output modules

plot_ui <- function(id){
  plotOutput(NS(id, "plot"))
}

plot_growth_ui <- function(id){
  plotOutput(NS(id, "plot_growth"))
}