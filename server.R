library(shiny)
library(hash)
library(xtable)
library(Rlab)

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

{
  data_distributions <- hash(
    "Normal" = hash(
      "desc_HTML" = '
    <div class="panel panel-body">
      <p>
        In probability theory, a <b>normal</b> (or <b>Gaussian</b> or <b>Gauss</b> or <b>Laplace–Gauss</b>) <b>distribution</b> is a type of <span class="text-success">continuous probability distribution</span> for a real-valued random variable. The general form of its probability density function is
      </p>
      <img src="https://wikimedia.org/api/rest_v1/media/math/render/svg/00cb9b2c9b866378626bcfa45c86a6de2f2b2e40" class="center-block" aria-hidden="true" style="vertical-align: -2.838ex; width:24.446ex; height:6.676ex;" />
      <br />
      <p>
        The parameter &mu; is the <span class="text-success">mean</span> or <span class="text-success">expectation</span> of the distribution (and also its <b>median</b> and <b>mode</b>), while the parameter &sigma; is its <span class="text-success">standard deviation</span>. The <span class="text-success">variance</span> of the distribution is &sigma;<sup>2</sup>.
      </p>
      
      <p>
        Normal distributions are important in statistics and are often used in the natural and social sciences to <b>represent real-valued random variables whose distributions are not known</b>. Their importance is partly due to the <span class="text-success">central limit theorem</span>. It states that, under some conditions, the average of many samples (observations) of a random variable with finite mean and variance is itself a random variable—whose distribution converges to a normal distribution as the number of samples increases. 
      </p>
    </div>
    
    ',
      "variables" = data.frame(
        "name" = c("&mu;", "&sigma;", "a", "b"),
        "input_id" = c("mean", "std_dev", "a", "b"),
        "default_value" = c(0, 1, -5, 5)
      )),
    "Uniform" = hash(
      "desc_HTML" = "The Uniform Distribution is lorem ipsum",
      "variables" = data.frame(
        "name" = c("a", "b"),
        "input_id" = c("a", "b"),
        "default_value" = c(0, 1)
      )),
    "Geometric" = hash(
      "desc_HTML" = '<div class="panel panel-body">
      <p>The geometric distribution represents the number of failures before you get a success in a series of 
      <a href="https://www.statisticshowto.com/bernoulli-distribution/#trial">Bernoulli trials</a>. 
      This <a href="https://www.statisticshowto.com/discrete-probability-distribution/">discrete probability distribution</a> 
      is represented by the 
      <a href="https://www.statisticshowto.com/probability-density-function/">probability density function</a>:</p>
      <br />
      <p>
        f(x) = (1 − p)<sup><strong>x</strong></sup> *p
      </p>
      <footer>
        <p>Origin of this description:
          <a href="https://www.statisticshowto.com/geometric-distribution/"> Statistics how to</a>
        </p>
      </footer>
    </div>',
      "variables" = data.frame(
        "name" = c("p", "a", "b"),
        "input_id" = c("p", "a", "b"),
        "default_value" = c(0.5, 0, 10),
        "min" = c(0.1, 0, 0),
        "max" = c(1, 99, 100),
        "step" = c(0.05, 1, 1)
      )
    ),
    "Bernoulli" = hash(
      "desc_HTML" = '<div class="panel panel-body">
        <p>A <b>Bernoulli distribution</b> is a <span class="text-success">discrete probability distribution</span> for a Bernoulli trial - a random experiment that has only two outcomes (usually called a "Success" or a "Failure"). For example, the probability of getting a heads (a "success") while flipping a coin is 0.5. The probability of "failure" is 1 - P (1 minus the probability of success, which also equals 0.5 for a coin toss). It is a special case of the <span class="text-success">binomial distribution</span> for n = 1. In other words, it is a binomial distribution with a single trial (e.g. a single coin toss).
        </p>
      </div>
      ',
      "variables" = data.frame(
        "name" = c("p","x"),
        "input_id" = c("p","x"),
        "default_value" = c(0.5,1),
        "min" = c(0,0),
        "max" = c(1,1),
        "step" = c(0.05,1)
      )
    ),
    "Binomial" = hash(
      "desc_HTML" = '<div class="panel panel-body">
       <p>In probability theory and statistics, the <b>binomial distribution</b> with parameters n and p is the <span class="text-success">discrete probability distribution</span> of the number of successes in a sequence of n <b>independent experiments</b>, each asking a yes-no question, and each with its own Boolean-valued outcome: <span class="text-success">success (with probability p) or failure (with probability q = 1 ??? p)</span>. The <b>binomial distribution</b> is a <b>Bernoulli distribution</b>. The binomial distribution is the basis for the popular binomial test of statistical significance.

The <b>binomial distribution</b> is frequently used to model <span class="text-success">the number of successes</span> in a sample of size n drawn with replacement from a population of size N. If the sampling is carried out without replacement, the draws are not independent and so the resulting distribution is a hypergeometric distribution, not a binomial one. However, for N much larger than n, the binomial distribution remains a good approximation, and is widely used. </p>
      </div>'
      ,
      "variables" = data.frame(
        "name" = c("p","n"),
        "input_id" = c("p","n"),
        "default_value" = c(0.5,1),
        "min" = c(0,1),
        "max" = c(1,20),
        "step" = c(0.05,1)
      )
    )
  )
}

function(input, output) {
  distribution_plot_data <- eventReactive(c(input$update_plot, input$n_points), {
    distribution_name = paste(input$distribution) # Concatenate vectors after converting to character.
    
    print(distribution_name)
    
    # if the data didn't load yet (update button hasn't been clicked)
    # fill in the data for N(0,1)
    if(length(distribution_name) == 0){
      distribution_name <- 'Normal'
      points <- seq(-5, 5, length.out = 20)
      
      hash(
        "desc" = data_distributions[[distribution_name]]$desc_HTML,
        "dens_mass_plot_values" = dnorm(points, 0, 1),
        "distribution_plot_values" = pnorm(points, 0, 1),
        "points" = points,
        "statistics" = hash(
          "Mean" = 0,
          "Median" = 0,
          "Variance" = 1,
          "Standard deviation" = 1
        ))
    }
    else {
      if(distribution_name == 'Normal') {
        points <- seq(input$var_a, input$var_b, length.out = input$n_points)
        std_dev <- input$var_std_dev
        mean <- input$var_mean
        
        hash(
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dnorm(points, mean, std_dev),
          "distribution_plot_values" = pnorm(points, mean, std_dev),
          "points" = points,
          "statistics" = hash(
            "Mean" = mean,
            "Median" = mean,
            "Variance" = std_dev,
            "Standard deviation" = sqrt(std_dev)
          ))
      }
      else if(distribution_name == 'Uniform') {
        
      }
      else if(distribution_name == 'Geometric') {
        p <- input$var_p
        if(p != 0) {
          points <- seq(input$var_a, input$var_b)
          print(points)
          
          var <- (1 - p)/ p ** 2
          mean <- 1 / p
          median <- -1 / log2(1 - p)
          
          hash(
            "desc" = data_distributions[[distribution_name]]$desc_HTML,
            "dens_mass_plot_values" = dgeom(points, prob = p),
            "distribution_plot_values" = pgeom(points, prob = p),
            "points" = points,
            "statistics" = hash(
              "Mean" = mean,
              "Median" = median,
              "Variance" = var,
              "Standard deviation" = sqrt(var)
            ))
        }
        else {
          # idk yet
        }
      }
      else if(distribution_name == 'Bernoulli'){
        if ( input$var_x ==0){
          p<- 1-(input$var_p)
        }
        else{
          p<- input$var_p
        }
          
        points <- seq(0,1)
        print(points)
        
        var <- p*(1-p)
        mean <- p
        if ( p < 1/2 ){
          median <- 0
        }
        else if ( p > 1/2){
          median <- 1
        }
        else{
          median <- "[0,1]"
        }
        hash(
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dbern(points,prob = p),
          "distribution_plot_values" = pbern(points,prob = p),
          "points" = points,
          "statistics" = hash(
            "Mean" = mean,
            "Median" = median,
            "Variance" = var,
            "Standard deviation" = sqrt(var)
          ))
          
      }
      else if(distribution_name == 'Binomial'){
        p<- (input$var_p)
        n<- (input$var_n)
        points<- seq(0,n)
        print(points)
        
        var <- n * p* (1-p)
        mean <- n* p
        median <- n *p
        hash(
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dbinom( points,size=n,prob=p),
          "distribution_plot_values" = pbinom( points,size=n,prob=p),
          "points" = points,
          "statistics" = hash(
            "Mean" = mean,
            "Median" = median,
            "Variance" = var,
            "Standard deviation" = sqrt(var)
          ))
      }
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
      
      min <- NA
      max <- NA
      step <- NA
      if(length(row) > 3) {
        min <- row[4]
        max <- row[5]
        step <- row[6]
      }
      
      numericInput(inputId = paste("var_", input_id, sep = ""), label = HTML(name), value = as.numeric(default_value), min = min, max = max, step = step)
    })
    
    # Create the checkboxes and select them all by default
  })
  
  output$desc <- renderText({distribution_plot_data()$desc})
  
  output$density_mass_plot <- renderPlot({
    data <- distribution_plot_data()
    plot(data$points, data$dens_mass_plot_values)
  })
  
  output$distribution_plot <- renderPlot({
    data <- distribution_plot_data()
    plot(data$points, data$distribution_plot_values)
  })
  
  output$statistics_table <- renderTable({
    data <- distribution_plot_data()
    
    # we have to convert our 'statistics' hash to a data frame so that it can be rendered to the UI
    table <- data.frame(matrix(ncol = 2, nrow = 0))
    col_names <- c("Stat", "Value")
    colnames(table) <- col_names
    
    for(key in keys(data$statistics))
    {
      table[nrow(table) + 1,] <- list(key, data$statistics[[key]])
    }
    
    table
  })
}