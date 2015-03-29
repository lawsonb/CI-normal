library(shiny)

# confidence interval app with plots in tabs
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Confidence Intervals for Mean and Variance of a Normal Distribution."),
  
  helpText("The random samples from which the CI's are generated do not change 
           until either the Refresh button is clicked or a parameter is changed.
           Therefore the tabs show results for the same data."),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label = h4("Confidence Intervals"),
                   choices = list("Mean" = 1, "Variance" = 2), 
                   selected = 1),
      actionButton("action", label="Refresh"),
      sliderInput("cr",
                  "Coverage rate:",min = 0.01,max = 0.99,value = 0.95),
      sliderInput("trials",
                  "Number of trials:",min = 10,max = 5000,value = 100),
      sliderInput("size",
                  "Samples in each trial:",min = 2,max = 500,value = 20),
      sliderInput("var",
                  "Variance:",min = 0.1,max = 100,value = 1),
      sliderInput("mean",
                  "Mean:",min = -10,max = 10,value = 0)
    ),    
  mainPanel(
    tabsetPanel(type = "tabs",  
                tabPanel("Mean & Variance Unknown", 
                         plotOutput("both.unknown.Plot")), 
                tabPanel("One Parameter Unknown",
                           plotOutput("one.unknown.Plot")),
                tabPanel("Interval Data", 
                         tableOutput("interval"))
    ))
  )
))
