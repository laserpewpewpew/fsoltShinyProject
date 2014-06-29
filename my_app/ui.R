library(shiny)
library(foreign)
swiid <- read.csv("SWIIDv5_0summary.csv", as.is=T)

shinyUI(fluidPage(
  titlePanel("Graphing Inequality"),
  
  sidebarLayout(position = "right",
    sidebarPanel(
      
                 selectInput("series", label="Series",
                             choices = list("Net Inequality" = "gini_net",
                                            "Market Inequality" = "gini_market"),
                             selected = "gini_net"),
                 
                 
                 
                 br(),
                 helpText("Choose up to 4 Countries:"),
                 selectInput("country1", label="Country 1",
                             choices = swiid$country,
                             selected = sample(swiid$country,1)),
                 selectInput("country2", label="Country 2",
                             choices = swiid$country,
                             selected = sample(swiid$country,1)),
                 selectInput("country3", label="Country 3",
                             choices = swiid$country,
                             selected = sample(swiid$country,1)),
                 selectInput("country4", label="Country 4",
                             choices = swiid$country,
                             selected = sample(swiid$country,1)),
                 
                 
                 
                 br(),
                 helpText("Choose a Range of Dates:"),
                 sliderInput("dates", label="Dates",
                             min = min(swiid$year), max = max(swiid$year), value = c(1975, max(swiid$year)), format = "####")
      ),
    mainPanel(
      plotOutput("plot"))
    )
))