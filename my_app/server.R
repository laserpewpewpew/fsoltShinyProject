library(shiny)
library(foreign)

shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
  
    library(foreign)
    swiid <- read.csv("SWIIDv5_0summary.csv", as.is=T)
    
    mydata <- swiid[swiid$country==input$country1 | swiid$country==input$country2 | swiid$country==input$country3 | swiid$country==input$country4,]
    
    ggplot(data=mydata, 
           aes(x=year, y=input$data, colour=country))
    + geom_line() +
      theme(legend.position="none") +
      coord_cartesian(xlim=c(input$dates[1],input$dates[2]),ylim = c(18, 40)) +
      labs(x = "Year", y = "SWIID Gini Index, Net Income") +
      geom_ribbon(aes(ymin = gini_net-1.96*gini_net_se, ymax = gini_net+1.96*gini_net_se, 
                      fill=country, linetype=NA), alpha = .25) +
      geom_text(aes(2002,33,label = "United Kingdom", colour="United Kingdom"), size=4.5) +
      geom_text(aes(2007,30,label = "Germany", colour="Germany"), size=4.5) +
      geom_text(aes(2003,22.3,label = "Sweden", colour="Sweden"), size=4.5) +
      geom_text(aes(2008,38.5,label = "United States", colour="United States"), size=4.5)
    
  })
    
}
)