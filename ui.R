library(shiny)

fluidPage(
  theme = "flatly.css",
  navbarPage(
    title = "Shiny RV Project",
    tabPanel("Distributions",
     titlePanel("Distributions of various variables"),
     sidebarLayout(
       sidebarPanel(
         style = "max-height: 70vh; overflow: scroll;",
         uiOutput("choose_distribution"),
         uiOutput("choose_values"),
         sliderInput(inputId = "n_points", min = 1, max = 100, value = 20, label = "No. points"),
         actionButton(
           inputId = "update_plot",
           label = "Update"),
       ),
       
       mainPanel(
         tabsetPanel(
           tabPanel("Description",
                    uiOutput(outputId = "desc")),
           tabPanel("Density/Mass Function", {
               plotOutput(
                outputId = "density_mass_plot"
                )
             }),
           tabPanel("Distribution Function", {
             plotOutput(
               outputId = "distribution_plot"
             )
           }),
           tabPanel("Various Statistics",
            tableOutput(outputId = "statistics_table"))
         )
       )
     )
    ),
    tabPanel("Test")
  )
)