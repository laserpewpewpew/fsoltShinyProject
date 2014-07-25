library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(gridExtra)
library(ggthemes)

swiid <- read.csv("SWIIDv5_0summary.csv", as.is=T)
ch4 <- list("Net Inequality" = "gini_net",
            "Market Inequality" = "gini_market",
            "Relative Redistribution" = "rel_red",
            "Absolute Redistribution" = "abs_red")
ch2 <- list("Net Inequality" = "gini_net",
            "Market Inequality" = "gini_market")
cc <- ddply(swiid, .(country), summarize, ch = ifelse(sum(!is.na(rel_red))>0, "ch4", "ch2" ))

shinyServer(function(input, output, session) {
  
#   updateSliderInput(session, "years", label="Select Years:",
#                             min = min(swiid$year, na.rm=T), max = max(swiid$year, na.rm=T), 
#                             value = c(1975, max(swiid$year, na.rm=T)), format = "####")
  
  observe({
    updateSelectInput(session, "country1", choices = c("select a country", cc$country), selected = "select a country")
  })
  observe({
    updateSelectInput(session, "country2", choices = c("select a country", cc$country), selected = "select a country")
  })
  observe({
    updateSelectInput(session, "country3", choices = c("select a country", cc$country), selected = "select a country")
  })
  observe({
    updateSelectInput(session, "country4", choices = c("select a country", cc$country), selected = "select a country")
  })
  
  observe({
    if(input$country1 != "select a country") {
      cc1 = cc[cc==input$country1, "ch"]
    } else cc1 <- "ch2"
    updateSelectInput(session, "series1", choices = get(cc1), selected="gini_net" )    
  })
  
  
  observe({
    if(input$country2 != "select a country") {
      cc2 = cc[cc==input$country2, "ch"]
    } else cc2 <- "ch2"
    updateSelectInput(session, "series2", choices = get(cc2), selected="gini_net" )    
  })
  
  
  observe({
    if(input$country3 != "select a country") {
      cc3 = cc[cc==input$country3, "ch"]
    } else cc3 <- "ch2"
    updateSelectInput(session, "series3", choices = get(cc3), selected="gini_net" )    
  })
  
  observe({
    if(input$country4 != "select a country") {
      cc4 = cc[cc==input$country4, "ch"]
    } else cc4 <- "ch2"
    updateSelectInput(session, "series4", choices = get(cc4), selected="gini_net" )    
  })
  
  output$plot <- renderPlot({
    if(input$country1 != "select a country"){     
      s1 <- data.frame(swiid[swiid$country==input$country1, 
                             c("country", "year", input$series1, paste0(input$series1, "_se"))])
      
      if(input$country2 != "select a country"){
        s2 <- data.frame(swiid[swiid$country==input$country2, 
                               c("country", "year", input$series2, paste0(input$series2, "_se"))])
        s1 <- merge(s1, s2, all=T)
        
        if(input$country3 != "select a country"){
          s2 <- data.frame(swiid[swiid$country==input$country3, 
                                 c("country", "year", input$series3, paste0(input$series3, "_se"))])
          s1 <- merge(s1, s2, all=T)
          if(input$country4 != "select a country"){
            s2 <- data.frame(swiid[swiid$country==input$country4, 
                                   c("country", "year", input$series4, paste0(input$series4, "_se"))])
            s1 <- merge(s1, s2, all=T)
          }   
        } 
      }
      
      s1 <- melt(s1, id.vars=c("country", "year"), na.rm=T)
      s2 <- s1[grepl("\\_se", s1$variable), c("country", "year", "value")]
      s1 <- s1[!grepl("\\_se", s1$variable), ]
      s1 <- cbind(s1, s2[, 3])
      names(s1)[5] <- "value_se"
      s1$variable <- gsub("gini_net", "Gini Index, Net Income", s1$variable)
      s1$variable <- gsub("gini_market", "Gini Index, Market Income", s1$variable)
      s1$variable <- gsub("rel_red", "Relative Redistribution", s1$variable)
      s1$variable <- gsub("abs_red", "Absolute Redistribution", s1$variable)
      if (length(table(s1$variable))==1) {
        ylabel <- paste("SWIID", s1$variable[1])
        s1$series <- s1$country
      } else ylabel <- ""
      if (length(table(s1$country))==1) {
        c.title <- paste("Inequality in", s1$country[1])
        s1$series <- s1$variable
      } else c.title <- "Legend"
      if (length(table(s1$variable))>1 & length(table(s1$country))>1) {
        s1$series <- paste(s1$country, s1$variable, sep=", ")
      }
      if (input$bw==FALSE) {
        print(arrangeGrob(
          ggplot(s1, aes(x=year, y=value, colour=series)) + 
            geom_line() +
            geom_ribbon(aes(ymin = value-1.96*value_se, ymax = value+1.96*value_se, 
                            fill=series, linetype=NA), alpha = .25) +
            coord_cartesian(xlim=c(input$dates[1],input$dates[2])) +
            labs(x = "Year", y = ylabel) + 
            scale_fill_discrete(name = c.title) + scale_colour_discrete(name = c.title),
          sub=textGrob("Source: Standardized World Income Inequality Database v5.0", x=0, hjust=-0.1, vjust=0.1, 
                       gp=gpar(fontsize=10)))      
        )
      } else {
        print(arrangeGrob(
          ggplot(s1, aes(x=year, y=value, colour=series)) + 
            geom_line() +
            geom_ribbon(aes(ymin = value-1.96*value_se, ymax = value+1.96*value_se, 
                            fill=series, linetype=NA), alpha = .25) +
            coord_cartesian(xlim=c(input$dates[1],input$dates[2])) +
            labs(x = "Year", y = ylabel) + 
            scale_fill_discrete(name = c.title) + scale_colour_discrete(name = c.title) +
            theme_bw(),
          sub=textGrob("Source: Standardized World Income Inequality Database v5.0", x=0, hjust=-0.1, vjust=0.1, 
                       gp=gpar(fontsize=10))) 
        )
      }
       
    } else {
      return()
    }
    
  })
  
})