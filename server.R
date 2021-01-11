library(shiny)
library(hash)

outputDir <- "distribution_data"

saveData <- function(data) {
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}

data_distributions <- hash(
  "Normal" = hash(
    "desc_HTML" = "The Normal Distribution is lorem ipsum!",
    "variables" = data.frame(
      "name" = c("&sigma;", "&mu;", "a", "b"),
      "input_id" = c("std_dev", "mean", "a", "b"),
      "default_value" = c(0, 1, -5, 5)
    )),
  "Uniform" = hash(
    "desc_HTML" = "The Uniform Distribution is lorem ipsum",
    "variables" = data.frame(
      "name" = c("a", "b"),
      "input_id" = c("a", "b"),
      "default_value" = c(0, 1)
    ))
)



function(input, output) {
  distribution_plot_data <- eventReactive(c(input$update_plot, input$dens_mass_n_points), {
    distribution_name = paste(input$distribution)
    
    # if the data didn't load yet (update button hasn't been clicked)
    # fill in the data for N(0,1)
    if(length(distribution_name) == 0){
      distribution_name <- 'Normal'
      points <- seq(-5, 5, length.out = 25)
      
      hash(
        "desc" = data_distributions[[distribution_name]]$desc_HTML,
        "plot_values" = dnorm(points, 0, 1),
        "points" = points)
    }
    else {
      if(distribution_name == 'Normal') {
        points <- seq(input$var_a, input$var_b, length.out = input$dens_mass_n_points)
        
        hash(
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "plot_values" = dnorm(points, input$var_std_dev, input$var_mean),
          "points" = points)
      }
      else if(distribution_name == 'Uniform') {
        
      }
      else {}
    }
    
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
    
    variable_inputs <- apply(selected_distribution$variables, 1, function(row){
      name <- row[1]
      input_id <- row[2]
      default_value <- row[3]
      
      numericInput(inputId = paste("var_", input_id, sep = ""), label = HTML(name), value = as.numeric(default_value))
    })
    
    # Create the checkboxes and select them all by default
  })
  
  output$desc <- renderText({distribution_plot_data()$desc})
  
  output$density_mass_plot <- renderPlot({
    data <- distribution_plot_data()
    plot(data$points, data$plot_values)
  })
}