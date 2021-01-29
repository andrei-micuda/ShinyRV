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
         br(),
         actionButton(
           inputId = "newdist",
           label = "New Distribution"),
         br(),
         actionButton(
           inputId = "showdist",
           label = "Show user created distributions")
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
              
           )
           ),
           
           tabPanel("Applying Function",
              div(style="display:inline-block; flex-grow: 1;",
              div(
                class = "d-flex align-items-center justify-space-between",
                div(
                  span("g(x)=",style ="padding-right: 10px;"),
                  div(
                    style="display:inline-block; flex-grow: 1;",
                    textInput(
                      inputId = "function_input",
                      label = "",
                      width = "100%")
                  ),
                  
                  actionButton(
                    inputId = "function_apply",
                    label = "=")
                  ,
                  br(),
                  "Variance",
                  verbatimTextOutput('var_output', placeholder = TRUE),
                  "Mean",
                  verbatimTextOutput('mean_output', placeholder = TRUE))
                      
                    ))
            ),
           tabPanel("Show a specific R.V.",
              span("R.V.=   ",style ="padding-right: 10px;"),
              div(style="display:inline-block; flex-grow: 1;",
                  textInput(
                    inputId = "function_input1",
                    label = "",
                    width = "100%"))
              ,{
                plotOutput(
                  outputId = "rv_hist"
                )
              }
              ))
       )
     )
    ),
    tabPanel("Discrete RV Operations",
        titlePanel("Random variable manipulation"),
        div(style="display:inline-block; flex-grow: 1;",
              textInput("var_x", label = "Values X"),
              textInput("prob_x", label = "Probabilities X")),
        br(),
        div(style="display:inline-block; flex-grow: 1;",
              textInput("var_y", label = "Values Y"),
              textInput("prob_y", label = "Probabilities Y")),
        br(),
        actionButton(
          inputId = "rv_update",
          label = "Update"),
        br(),
        tableOutput(outputId = "rv_operations")
    ),
    tabPanel("Working with Events",
      titlePanel ("Please select the relationship between the events"),
      uiOutput("choose_relation"),
      uiOutput("events_calculator"),
      
      br(),"P(a",HTML("&cap;"),"b)",
      verbatimTextOutput('Intersectie', placeholder = TRUE),
      br(),"P(a U b)",
      verbatimTextOutput('Reuniune', placeholder = TRUE),
      br(),"P(b|a)",
      verbatimTextOutput('Restrictie', placeholder = TRUE)
      
      )
  )
)