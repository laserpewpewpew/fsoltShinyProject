library(shiny)

shinyUI(fluidPage(  
  sidebarLayout(position = "right",
                sidebarPanel(          
                  
                  helpText("Choose up to 4:"),
                  selectInput("country1", label="Country", "select a country"),
                                    
                  conditionalPanel(
                    condition = "input.country1 != 'select a country'",
                    selectInput("series1", label="Variable", 
                                choices = list("Net Inequality" = "gini_net"), 
                                selected = "gini_net"),
                    
                    selectInput("country2", label="Country2", "select a country")
                  ),
                  
                  conditionalPanel(
                    condition = "input.country2 != 'select a country'",
                    selectInput("series2", label="Variable",
                                choices = list("Net Inequality" = "gini_net"), 
                                selected = "gini_net"),
                    
                    selectInput("country3", label="Country3", "select a country")
                  ),
                  
                  conditionalPanel(
                    condition = "input.country3 != 'select a country'",
                    selectInput("series3", label="Variable",
                                choices = list("Net Inequality" = "gini_net",
                                               "Market Inequality" = "gini_market",
                                               "Relative Redistribution" = "rel_red",
                                               "Absolute Redistribution" = "abs_red"),
                                selected = "gini_net"),
                    
                    selectInput("country4", label="Country4", "select a country")
                  ), 
                  
                  conditionalPanel(
                    condition = "input.country4 != 'select a country'",
                    selectInput("series4", label="Variable",
                                choices = list("Net Inequality" = "gini_net"),
                                selected = "gini_net")
                  ),
                  br(),
                  sliderInput("dates", label="Select Years:",
                              min = min(swiid$year, na.rm=T), max = max(swiid$year, na.rm=T), 
                              value = c(1975, max(swiid$year, na.rm=T)), format = "####"),
                
                  checkboxInput("bw", "Black and White Background", FALSE)
                  ),
                          
                mainPanel(
                  plotOutput("plot")
                )
  )
))