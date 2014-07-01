library(shiny)
library(foreign)
swiid <- read.csv("SWIIDv5_0summary.csv", as.is=T)

shinyUI(fluidPage(
  titlePanel("Graphing SWIID"),
  
  sidebarLayout(position = "right",
    sidebarPanel(
      selectInput("compare", label="Comparison:",
                  choices=list("Within One Country" ="Within One Country",
                               "Across Multiple Countries"="Across Multiple Countries",
                               "Not Selected" = "Not Selected"),
                  selected = "Not Selected"),
      
      conditionalPanel(
        condition = "input$compare == Within One Country",
        checkboxGroupInput("ys", "Choose Series:",
                           choices=list("Net Inequality" = "gini_net",
                                        "Market Inequality" = "gini_market",
                                        "Relative Redistribution" = "rel_red",
                                        "Absolute Redistribution" = "abs_red"),
                           selected="gini_net"),
        selectInput("country", "Choose Country:",
                    choices=swiid$country,
                    selected=sample(1:170,1)),
        sliderInput("dates", label="Dates",
                    min = min(swiid$year, na.rm=T), 
                    max = max(swiid$year, na.rm=T), 
                    value = c(1975, max(swiid$year, 
                                        na.rm=T)), 
                    format = "####")
      ),
      
      conditionalPanel(
        condition = "input$compare == Across Multiple Countries",
        selectInput("series", label="Series",
                    choices = list("Net Inequality" = "gini_net",
                                   "Market Inequality" = "gini_market",
                                   "Relative Redistribution" = "rel_red",
                                   "Absolute Redistribution" = "abs_red"),
                    selected = "gini_net"),
        
        br(),
        helpText("Choose up to 4 Countries:"),
        selectInput("country1", label="Country 1",
                    choices = swiid$country,
                    selected = sample(1:170,1)),
        
        selectInput("country2", label="Country 2",
                    choices = swiid$country,
                    selected = sample(1:170,1)),
        
        selectInput("country3", label="Country 3",
                    choices = swiid$country,
                    selected = sample(1:170,1)),
        
        selectInput("country4", label="Country 4",
                    choices = swiid$country,
                    selected = sample(1:170,1)),
        
        br(),
        helpText("Choose a Range of Dates:"),
        sliderInput("dates", label="Dates",
                    min = min(swiid$year, na.rm=T), max = max(swiid$year, na.rm=T), value = c(1975, max(swiid$year, na.rm=T)), format = "####")
        )
    ),
      mainPanel(
      plotOutput("plot"))
    )
))