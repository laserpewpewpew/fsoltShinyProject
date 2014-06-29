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
                             selected = "Canada"),
                 selectInput("country2", label="Country 2",
                             choices = swiid$country,
                             selected = "United States"),
                 selectInput("country3", label="Country 3",
                             choices = swiid$country,
                             selected = "United Kingdom"),
                 selectInput("country4", label="Country 4",
                             choices = swiid$country,
                             selected = "Germany"),
                 
                 
                 br(),
                 helpText("Choose a Range of Dates:"),
                 sliderInput("dates", label="Dates",
                             min = min(swiid$year, na.rm=T), max = max(swiid$year, na.rm=T), value = c(1975, max(swiid$year, na.rm=T)), format = "####")
      ),
    mainPanel(
      plotOutput("plot"))
    )
))