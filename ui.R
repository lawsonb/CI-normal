library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Confidence Intervals for Mean and Variance of a Normal Distribution."),
  
  helpText("The random samples from which the CI's are generated do not change 
           until either the Refresh button is clicked or a parameter is changed.
           Therefore the tabs show results for the same data. When the coverage
           rate is changed new CIs are calculated for the existing data.  When 
           changing between displaying CIs for mean and variance the existing
           data is used."),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label = h4("Confidence Intervals"),
                   choices = list("Mean" = 1, "Variance" = 2), 
                   selected = 1),
      actionButton("action", label="Refresh"),
      sliderInput("cr",
                  "Coverage rate:",min = 0.01,max = 0.99,value = 0.95),
      sliderInput("trials",
                  "Number of trials:",min = 10,max = 1000,value = 100),
      sliderInput("size",
                  "Samples in each trial:",min = 2,max = 10000,value = 100),
      sliderInput("var",
                  "Variance:",min = 0.1,max = 100,value = 1),
      sliderInput("mean",
                  "Mean:",min = -10,max = 10,value = 0)
    ),    
    mainPanel(
      tabsetPanel(type = "tabs",  
                tabPanel("Mean & Variance Unknown", 
                         plotOutput("both.unknown.Plot")), 
                tabPanel("Both Unknown Hist QQ", 
                         plotOutput("both.unknown.HistQQ")), 
                tabPanel("One Parameter Unknown",
                           plotOutput("one.unknown.Plot")),
                tabPanel("Interval Data", 
                         tableOutput("interval"))
      ),
      br(),
      p("Code ",
        a("here,", 
          href = "https://github.com/lawsonb/CI-normal", 
          target="_blank"),
        " equations for confidence intervals ",
        a("here.", 
          href = "https://github.com/lawsonb/CI-normal/blob/master/CI-normal.pdf", 
          target="_blank")
      )
    )
  )
))
