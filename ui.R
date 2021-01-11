library(shiny)
library(shinythemes)

fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    title = "Shiny RV Project",
    tabPanel("Distributions",
     titlePanel("Distributions of various variables"),
     sidebarLayout(
       sidebarPanel(
         uiOutput("choose_distribution"),
         uiOutput("choose_values"),
         actionButton(
           inputId = "update_plot",
           label = "Update"),
       ),
       
       mainPanel(
         tabsetPanel(
           tabPanel("Description",
                    textOutput(outputId = "desc")),
           tabPanel("Density/Mass Function",
             fluidRow(
               plotOutput(
                outputId = "density_mass_plot"
             )),
             fluidRow(
               column(6,
                sliderInput(inputId = "dens_mass_n_points", min = 1, max = 100, value = 20, label = "No. points"),
                offset = 3)
             )),
           tabPanel("Distribution Function", {
             plotOutput(
               outputId = "distribution_plot"
             )
           }),
           tabPanel("Debug", {
             textOutput(outputId = "debug")
           })
         )
       )
     )
    ),
    tabPanel("Test")
  )
)