library(shiny)
library(hash)

data_distributions <- hash(
  "Normal" = data.frame(
    "desc_HTML" = "The Normal Distribution is lorem ipsum",
    "variable" = c("&sigma;", "&mu;", "a", "b"),
    "input_id" = c("std_dev", "mean", "a", "b"),
    "default_value" = c( 0, 1, -5, 5)),
  "Uniform" = data.frame(
    "variable" = c("a", "b", "c", "test"),
    "input_id" = c("a", "b", "c", "test"),
    "default_value" = c(0, 1, 5, 12))
)


function(input, output) {
  distribution_plot_data <- eventReactive(input$update_plot, {
    distribution_name = paste(input$distribution)
    if(distribution_name == 'Normal') {
      points <- seq(input$var_a, input$var_b, length.out = 20)
      
      data.frame(
        "desc" = data_distributions[[distribution_name]]$desc_HTML,
        "plot_values" = dnorm(points, input$var_std_dev, input$var_mean),
        "points" = points)
    }
    else if(distribution_name == 'Uniform') {
      
    }
    else {}
    
  })
  
  # Drop-down selection box for which data set
  output$choose_distribution <- renderUI({
    selectInput(inputId = "distribution", "Distribution", keys(data_distributions))
  })
  
  # Inputs for the variables of the selected distribution
  output$choose_values <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$distribution))
      return()
    
    # Get the data set with the appropriate name
    selected_distribution <- data_distributions[[paste(input$distribution)]]
    
    variable_inputs <- lapply(seq_along(selected_distribution[,1]), function(i){
      numericInput(inputId = paste("var_", selected_distribution[i,]$input_id, sep = ""), label = HTML(selected_distribution[i,]$variable), value = selected_distribution[i,]$default_value)
    })
    
    # Create the checkboxes and select them all by default
  })
  
  output$desc <- renderText(distribution_plot_data()$desc)
  
  output$density_mass_plot <- renderPlot({
    data <- distribution_plot_data()
    plot(data$points, data$plot_values)
  })

  output$debug <- renderText(input$var_a)
}