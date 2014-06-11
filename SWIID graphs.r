library(ggplot2)

swiid <- read.csv("SWIIDv4_1summary.csv", as.is=T)


s4 <- swiid[swiid$country=="United States" | swiid$country=="United Kingdom" | swiid$country=="Sweden" | swiid$country=="Germany",]

pdf(file="4c.pdf",width=8, height=5.25)
ggplot(data=s4, aes(x=year, y=gini_net, colour=country)) + geom_line() +
	theme(legend.position="none") +    
	coord_cartesian(xlim=c(1969,2012),ylim = c(18, 40)) +
	labs(x = "Year", y = "SWIID Gini Index, Net Income") +
    geom_ribbon(aes(ymin = gini_net-1.96*gini_net_se, ymax = gini_net+1.96*gini_net_se, 
    	fill=country, linetype=NA), alpha = .25) +
    geom_text(aes(2002,33,label = "United Kingdom", colour="United Kingdom"), size=4.5) +
    geom_text(aes(2007,30,label = "Germany", colour="Germany"), size=4.5) +
    geom_text(aes(2003,22.3,label = "Sweden", colour="Sweden"), size=4.5) +
	geom_text(aes(2008,38.5,label = "United States", colour="United States"), size=4.5)
graphics.off()


latam <- swiid[swiid$country=="United Kingdom" | swiid$country=="Chile" | 
	swiid$country=="Brazil" | swiid$country=="Mexico" | swiid$country=="Uruguay",]

pdf(file="latam.pdf",width=8, height=5.25)
ggplot(data=latam, aes(x=year, y=gini_net, colour=country)) + geom_line() +
	theme(legend.position="none") +    
	coord_cartesian(xlim=c(1970,2012),ylim = c(25, 65)) +
	labs(x = "Year", y = "SWIID Gini Index, Net Income") +
    geom_ribbon(aes(ymin = gini_net-1.96*gini_net_se, ymax = gini_net+1.96*gini_net_se, 
    	fill=country, linetype=NA), alpha = .25) +
    geom_text(aes(2002,32.8,label = "United Kingdom", colour="United Kingdom"), size=4.5) +
    geom_text(aes(1989,55.5,label = "Brazil", colour="Brazil"), size=4.5) +
    geom_text(aes(2010,50,label = "Chile", colour="Chile"), size=4.5) +
	geom_text(aes(1996,45.5,label = "Mexico", colour="Mexico"), size=4.5) +
	geom_text(aes(1999,41,label = "Uruguay", colour="Uruguay"), size=4.5)
graphics.off()

africa <- swiid[swiid$country=="Brazil" | swiid$country=="Egypt" | 
	swiid$country=="South Africa" | swiid$country=="Nigeria",]

africa <- africa[africa$year>1974,]

pdf(file="africa.pdf",width=8, height=5.25)
ggplot(data=africa, aes(x=year, y=gini_net, colour=country)) + geom_line() +
	theme(legend.position="none") +    
	coord_cartesian(xlim=c(1970,2012),ylim = c(25, 65)) +
	labs(x = "Year", y = "SWIID Gini Index, Net Income") +
    geom_ribbon(aes(ymin = gini_net-1.96*gini_net_se, ymax = gini_net+1.96*gini_net_se, 
    	fill=country, linetype=NA), alpha = .25) +
    geom_text(aes(1976,51,label = "Brazil", colour="Brazil"), size=4.5) +
    geom_text(aes(2008,61.5,label = "South Africa", colour="South Africa"), size=4.5) +
 	geom_text(aes(2008,37.5,label = "Nigeria", colour="Nigeria"), size=4.5) +
	geom_text(aes(2005,28,label = "Egypt", colour="Egypt"), size=4.5)
graphics.off()


brics <- swiid[swiid$country=="Brazil" | (swiid$country=="Russian Federation") | 
	(swiid$country=="India" & swiid$year>=1974) | swiid$country=="China",]

pdf(file="brics.pdf",width=8, height=5.25)
ggplot(data=brics, aes(x=year, y=gini_net, colour=country)) + geom_line() +
	theme(legend.position="none") +    
	coord_cartesian(xlim=c(1970,2012),ylim = c(20, 65)) +
	labs(x = "Year", y = "SWIID Gini Index, Net Income") +
    geom_ribbon(aes(ymin = gini_net-1.96*gini_net_se, ymax = gini_net+1.96*gini_net_se, 
    	fill=country, linetype=NA), alpha = .25) +
    geom_text(aes(1974.5,63,label = "Brazil", colour="Brazil"), size=4.5) +
    geom_text(aes(2000,57.5,label = "India", colour="India"), size=4.5) +
 	geom_text(aes(2000,38,label = "Russia", colour="Russia"), size=4.5) +
	geom_text(aes(1980,35,label = "China", colour="China"), size=4.5)
graphics.off()


########
#ICCFR
########

eng <- swiid[swiid$country=="United States" | swiid$country=="United Kingdom" | swiid$country=="Australia" | swiid$country=="Canada",]

pdf(file="eng.pdf",width=8, height=5.25)
ggplot(data=eng, aes(x=year, y=gini_net, colour=country)) + geom_line() +
	theme(legend.position="none") +    
	coord_cartesian(xlim=c(1979,2012),ylim = c(18, 40)) +
	labs(x = "Year", y = "SWIID Gini Index, Net Income") +
    geom_ribbon(aes(ymin = gini_net-1.96*gini_net_se, ymax = gini_net+1.96*gini_net_se, 
    	fill=country, linetype=NA), alpha = .25) +
    geom_text(aes(2000,35.5,label = "United Kingdom", colour="United Kingdom"), size=4.5) +
    geom_text(aes(2009,30,label = "Canada", colour="Canada"), size=4.5) +
    geom_text(aes(2005,33,label = "Australia", colour="Australia"), size=4.5) +
	geom_text(aes(2008,38.5,label = "United States", colour="United States"), size=4.5)
graphics.off()


euro <- swiid[swiid$country=="United States" | swiid$country=="Netherlands" | swiid$country=="Sweden" | swiid$country=="Germany" | swiid$country=="France" ,]

pdf(file="euro.pdf",width=8, height=5.25)
ggplot(data=euro, aes(x=year, y=gini_net, colour=country)) + geom_line() +
	theme(legend.position="none") +    
	coord_cartesian(xlim=c(1979,2012),ylim = c(18, 40)) +
	labs(x = "Year", y = "SWIID Gini Index, Net Income") +
    geom_ribbon(aes(ymin = gini_net-1.96*gini_net_se, ymax = gini_net+1.96*gini_net_se, 
    	fill=country, linetype=NA), alpha = .25) +
    geom_text(aes(1991,24,label = "Netherlands", colour="Netherlands"), size=4.5) +
    geom_text(aes(1982,34,label = "France", colour="France"), size=4.5) +
    geom_text(aes(2007,30,label = "Germany", colour="Germany"), size=4.5) +
    geom_text(aes(2003,22.3,label = "Sweden", colour="Sweden"), size=4.5) +
	geom_text(aes(2008,38.5,label = "United States", colour="United States"), size=4.5)
graphics.off()


ee <- swiid[swiid$country=="United States" | swiid$country=="Poland" | swiid$country=="Czech Republic" | swiid$country=="Latvia" | swiid$country=="Hungary" ,]

pdf(file="ee.pdf",width=8, height=5.25)
ggplot(data=ee, aes(x=year, y=gini_net, colour=country)) + geom_line() +
	theme(legend.position="none") +    
	coord_cartesian(xlim=c(1979,2012),ylim = c(18, 40)) +
	labs(x = "Year", y = "SWIID Gini Index, Net Income") +
    geom_ribbon(aes(ymin = gini_net-1.96*gini_net_se, ymax = gini_net+1.96*gini_net_se, 
    	fill=country, linetype=NA), alpha = .25) +
    geom_text(aes(2006,32,label = "Poland", colour="Poland"), size=4.5) +
    geom_text(aes(1991.5,32,label = "Hungary", colour="Hungary"), size=4.5) +
    geom_text(aes(2010,33.5,label = "Latvia", colour="Latvia"), size=4.5) +
    geom_text(aes(2008.5,23.5,label = "Czech Republic", colour="Czech Republic"), size=4.5) +
	geom_text(aes(2008,38.5,label = "United States", colour="United States"), size=4.5)
graphics.off()



asia <- swiid[swiid$country=="United States" | swiid$country=="Korea, Republic of" | swiid$country=="Japan" | swiid$country=="Taiwan",]

pdf(file="asia.pdf",width=8, height=5.25)
ggplot(data=asia, aes(x=year, y=gini_net, colour=country)) + geom_line() +
	theme(legend.position="none") +    
	coord_cartesian(xlim=c(1979,2012),ylim = c(18, 40)) +
	labs(x = "Year", y = "SWIID Gini Index, Net Income") +
    geom_ribbon(aes(ymin = gini_net-1.96*gini_net_se, ymax = gini_net+1.96*gini_net_se, 
    	fill=country, linetype=NA), alpha = .25) +
    geom_text(aes(1997,25,label = "Japan", colour="Japan"), size=4.5) +
    geom_text(aes(1989,28,label = "Taiwan", colour="Taiwan"), size=4.5) +
    geom_text(aes(2004,33,label = "Korea", colour="Korea, Republic of"), size=4.5) +
	geom_text(aes(2008,38.5,label = "United States", colour="United States"), size=4.5)
graphics.off()


latam <- swiid[swiid$country=="United States" | swiid$country=="Chile" | 
	swiid$country=="Brazil" | swiid$country=="Mexico" | swiid$country=="Uruguay",]

pdf(file="latam.pdf",width=8, height=5.25)
ggplot(data=latam, aes(x=year, y=gini_net, colour=country)) + geom_line() +
	theme(legend.position="none") +    
	coord_cartesian(xlim=c(1979,2012),ylim = c(25, 65)) +
	labs(x = "Year", y = "SWIID Gini Index, Net Income") +
    geom_ribbon(aes(ymin = gini_net-1.96*gini_net_se, ymax = gini_net+1.96*gini_net_se, 
    	fill=country, linetype=NA), alpha = .25) +
    geom_text(aes(2002,35,label = "United States", colour="United States"), size=4.5) +
    geom_text(aes(1989,55.5,label = "Brazil", colour="Brazil"), size=4.5) +
    geom_text(aes(2010,50,label = "Chile", colour="Chile"), size=4.5) +
	geom_text(aes(1996,45.5,label = "Mexico", colour="Mexico"), size=4.5) +
	geom_text(aes(1999,41,label = "Uruguay", colour="Uruguay"), size=4.5)
graphics.off()
