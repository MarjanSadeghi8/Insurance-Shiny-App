# STAT 6366 - Homework 5
# Christine Jator, Marjan Sadeghi
# Medical Insurance Shiny App
# Data: https://www.kaggle.com/datasets/mirichoi0218/insurance
# Save insurance.csv in the same folder as this file.

library(shiny)
library(plotly)
library(bslib)

insurance <- read.csv("insurance.csv")

# These are the three numerical variables we want to explore.
variables <- c("Age" = "age", "BMI" = "bmi", "Charges" = "charges")

ui <- navbarPage(
  "Medical Insurance Explorer",
  theme = bs_theme(bootswatch = "flatly"),

  # TAB 1: HOME
  tabPanel("Home",
    fluidPage(
      h2("Medical Insurance Cost Explorer"),
      p("This app uses the Medical Cost Personal Dataset from Kaggle."),
      p("The dataset includes several variables: age, sex, Body Mass Index (BMI), number of children covered by health insurance,
      smoking status, region, and individual medical costs billed by health insurance (charges)."),
      p("We explore how age and BMI are related to medical charges."),
      h3("How to use this app"),
      tags$ol(
        tags$li("Distribution: choose a variable and change the number of histogram bins."),
        tags$li("Scatterplot: choose X, Y, and color variables; use sliders to filter the data."),
        tags$li("3D Mesh: rotate the plot to explore age, BMI, and charges together.")
      ),
      h3("Interesting features"),
      p("Interactive Plotly charts provide hovering and zooming capabilities.
       Range filters allow for thorough visualizations. Several user inputs for easily accessible comparisons and visualizations."),
      p("Each observation in the dataset represents one person. Keep in mind: the charts reveal associations but not causations."),
      tags$a("View the dataset on Kaggle",
             href = "https://www.kaggle.com/datasets/mirichoi0218/insurance",
             target = "_blank")
    )
  ),

  # TAB 2: DISTRIBUTION
  tabPanel("Distribution",
    sidebarLayout(
      sidebarPanel(
        radioButtons("hist_var", "Choose a variable:",
                     choices = variables, selected = "charges"),
        sliderInput("bins", "Number of bins:",
                    min = 5, max = 70, value = 25)
      ),
      mainPanel(
        h3("Distribution of Insurance Variables"),
        plotlyOutput("histogram", height = "450px"),
        tableOutput("summary")
      )
    )
  ),

  # TAB 3: SCATTERPLOT
  tabPanel("Scatterplot",
    sidebarLayout(
      sidebarPanel(
        selectInput("xvar", "X-axis:", choices = variables, selected = "age"),
        selectInput("yvar", "Y-axis:", choices = variables, selected = "charges"),
        selectInput("colorvar", "Color by:",
                    choices = c(variables, "Smoker" = "smoker", "Sex" = "sex", "Region" = "region"),
                    selected = "smoker"),
        sliderInput("age", "Age range:",
                    min = min(insurance$age), max = max(insurance$age),
                    value = range(insurance$age),step = 1),
        sliderInput("bmi", "BMI range:",
                    min = floor(min(insurance$bmi)), max = ceiling(max(insurance$bmi)),
                    value = c(floor(min(insurance$bmi)), ceiling(max(insurance$bmi))), step = 0.5),
        sliderInput("charges", "Charges range ($):",
                    min = floor(min(insurance$charges)), max = ceiling(max(insurance$charges)),
                    value = c(floor(min(insurance$charges)), ceiling(max(insurance$charges))),
                    step = 500)
      ),
      mainPanel(
        h3("Scatterplot"),
        plotlyOutput("scatterplot", height = "500px"),
        textOutput("count")
      )
    )
  ),

  # TAB 4: 3D MESH
  tabPanel("3D Mesh",
    fluidPage(
      h3("3D Mesh: Age, BMI, and Medical Charges"),
      p("Drag to rotate the plot and scroll to zoom in or out."),
      p("The 3D mesh plot how age and BMI relate to the charges of medical health insurance. The 3D nature of the mesh plot provides a boundary view of this relation."),
      plotlyOutput("mesh", height = "600px")
    )
  )
)

server <- function(input, output, session) {

  # Histogram: the variable and number of bins come from the user.
  output$histogram <- renderPlotly({
    plot_ly(x = insurance[[input$hist_var]], type = "histogram",
            nbinsx = input$bins,  marker = list(
              line = list(color = "black", width = 1)
            )) %>%
      layout(xaxis = list(title = names(variables)[variables == input$hist_var]),
             yaxis = list(title = "Number of People"))
  })

  output$summary <- renderTable({
    x <- insurance[[input$hist_var]]
    data.frame(
      Mean = round(mean(x), 2),
      Median = round(median(x), 2),
      SD = round(sd(x), 2),
      Minimum = round(min(x), 2),
      Maximum = round(max(x), 2)
    )
  })

  # Update the data when the user changes any of the three sliders.
  filtered_data <- reactive({
    insurance[
      insurance$age >= input$age[1] & insurance$age <= input$age[2] &
      insurance$bmi >= input$bmi[1] & insurance$bmi <= input$bmi[2] &
      insurance$charges >= input$charges[1] & insurance$charges <= input$charges[2],
    ]
  })

  output$scatterplot <- renderPlotly({
    d <- filtered_data()
    validate(need(nrow(d) > 0, "No data for these ranges. Try moving the sliders."))
    validate(need(input$xvar != input$yvar, "Choose different X and Y variables."))

    # A factor gives category colors; numeric variables give a color scale.
    color_values <- d[[input$colorvar]]
    if (input$colorvar %in% c("Smoker", "Sex", "Region")) {
      color_values <- as.factor(color_values)
    }

    plot_ly(d,
            x = d[[input$xvar]],
            y = d[[input$yvar]],
            color = color_values,
            type = "scatter", mode = "markers",
            text = paste("Age:", d$age,
                         "<br>BMI:", round(d$bmi, 1),
                         "<br>Charges: $", round(d$charges, 2),
                         "<br>Smoker:", d$smoker),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = names(variables)[variables == input$xvar]),
             yaxis = list(title = names(variables)[variables == input$yvar]))
  })

  output$count <- renderText({
    paste("Showing", nrow(filtered_data()), "of", nrow(insurance), "People")
  })

  # The 3D mesh uses all of the data (not the scatterplot filters).
  output$mesh <- renderPlotly({
    plot_ly() %>%
      add_trace(x = insurance$age, y = insurance$bmi, z = insurance$charges,
                type = "mesh3d", alphahull = 0, opacity = 0.2,
                name = "Mesh", hoverinfo = "skip") %>%
      add_trace(x = insurance$age, y = insurance$bmi, z = insurance$charges,
        type = "scatter3d", mode = "markers", marker = list( size = 3, color = insurance$charges,
          colorscale = "Viridis",
          showscale = TRUE,
          colorbar = list(title = "Charges ($)")
        ))%>%
      layout(scene = list(
        xaxis = list(title = "Age"),
        yaxis = list(title = "BMI"),
        zaxis = list(title = "Charges ($)")
      ))
  })
}

shinyApp(ui, server)