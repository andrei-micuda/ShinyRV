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
            tableOutput(outputId = "statistics_table")),
           tabPanel("Probability calculator",
            div(
              class = "d-flex align-items-center justify-space-between",
               div(
                 class = "d-flex align-items-center probability_calc_input_wrapper",
                  span("P(", style="padding-right: 10px;"),
                  div(
                    style="display:inline-block; flex-grow: 1;",
                    textInput(
                      inputId = "probability_calc_input",
                      label = "",
                      width = "100%")),
                  span(")", style="padding-left: 10px; padding-right: 5px;")
                ),
               actionButton(
                 inputId = "calculate_probability",
                 label = "=")
              ,
                verbatimTextOutput('probability_calc_output', placeholder = TRUE)
           ))
         )
       )
     )
    ),
    tabPanel("Test")
  )
)