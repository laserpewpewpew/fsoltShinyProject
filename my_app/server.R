library(shiny)
library(foreign)
library(car)
library(ggplot2)
swiid <- read.csv("SWIIDv5_0summary.csv", as.is=T)

shinyServer(function(input, output) {

  
  output$plot <- renderPlot({
    
    if(input$country1=="Not Selected"){
      data.1 <- data.frame(swiid[swiid$country=="Not Selected",])
    } else{
      data.1 <- data.frame(swiid[swiid$country==input$country1,])
    }
    
    
    if(input$country2=="Not Selected"){
      data.2 <- data.frame(swiid[swiid$country=="Not Selected",])
    } else{
      data.2 <- data.frame(swiid[swiid$country==input$country2,])
    }
    
    
    if(input$country3=="Not Selected"){
      data.3 <- data.frame(swiid[swiid$country=="Not Selected",])
    } else{
      data.3 <- data.frame(swiid[swiid$country==input$country3,])
    }
    
    
    if(input$country4=="Not Selected"){
      data.4 <- data.frame(swiid[swiid$country=="Not Selected",])
    } else{
      data.4 <- data.frame(swiid[swiid$country==input$country4,])
    }
    
    data.5 <- rbind(data.1,data.2)
    data.6 <- rbind(data.3,data.4)
    total <- rbind(data.5,data.6)
    
    
    if(input$series == "gini_net"){
    print(
      ggplot(total, aes(x=year, y=gini_net, colour=country)) + 
        geom_line() +
        geom_ribbon(aes(ymin = gini_net-1.96*gini_net_se, ymax = gini_net+1.96*gini_net_se, 
                                    fill=country, linetype=NA), alpha = .25) +
        coord_cartesian(xlim=c(input$dates[1],input$dates[2])) +
        labs(x = "Year", y = "SWIID Gini Index, Net Income")
      )
    }
    else if(input$series == "market_net"){
      print(
        ggplot(total, aes(x=year, y=gini_market, colour=country)) + 
          geom_line() +
          geom_ribbon(aes(ymin = gini_market-1.96*gini_market_se, ymax = gini_market+1.96*gini_market_se, 
                          fill=country, linetype=NA), alpha = .25) +
          coord_cartesian(xlim=c(input$dates[1],input$dates[2])) +
          labs(x = "Year", y = "SWIID Gini Index, Market Income")
        )
    }
    else if(input$series == "rel_red"){
      print(
        ggplot(total, aes(x=year, y=rel_red, colour=country)) + 
          geom_line() +
          geom_ribbon(aes(ymin = rel_red-1.96*rel_red_se, ymax = rel_red+1.96*rel_red_se, 
                          fill=country, linetype=NA), alpha = .25) +
          coord_cartesian(xlim=c(input$dates[1],input$dates[2])) +
          labs(x = "Year", y = "SWIID Gini Index, Relative Redistribution")
      )
    }
    else if(input$series == "abs_red"){
      print(
        ggplot(total, aes(x=year, y=abs_red, colour=country)) + 
          geom_line() +
          geom_ribbon(aes(ymin = abs_red-1.96*abs_red_se, ymax = abs_red+1.96*abs_red_se, 
                          fill=country, linetype=NA), alpha = .25) +
          coord_cartesian(xlim=c(input$dates[1],input$dates[2])) +
          labs(x = "Year", y = "SWIID Gini Index, Absolute Redistribution")
      )
    }
    
  })
    
}
)