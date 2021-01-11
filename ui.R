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