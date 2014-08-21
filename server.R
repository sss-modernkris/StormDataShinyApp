# server.R

library(quantmod)
# source("helpers.R")

require(R.utils)
require(lubridate)
require(graphics)
# setInternet2(use = TRUE)

# data for USA
fatu <- readRDS("fatu.rds")
inju <- readRDS("inju.rds")
cropdmgu <- readRDS("cropdmgu.rds")
propdmgu <- readRDS("propdmgu.rds")

fatms <- readRDS("fatms.rds")
injms <- readRDS("injms.rds")
cropdmgms <- readRDS("cropdmgms.rds")
propdmgms <- readRDS("propdmgms.rds")

# data for State
fatss <- readRDS("fatss.rds")
injss <- readRDS("injss.rds")
cropdmgss <- readRDS("cropdmgss.rds")
propdmgss <- readRDS("propdmgss.rds")

fatsms <- readRDS("fatsms.rds")
injsms <- readRDS("injsms.rds")
cropdmgsms <- readRDS("cropdmgsms.rds")
propdmgsms <- readRDS("propdmgsms.rds")

# data for County
fatssc <- readRDS("fatssc.rds")
injssc <- readRDS("injssc.rds")
cropdmgssc <- readRDS("cropdmgssc.rds")
propdmgssc <- readRDS("propdmgssc.rds")

fatsmcs <- readRDS("fatsmcs.rds")
injsmcs <- readRDS("injsmcs.rds")
cropdmgsmcs <- readRDS("cropdmgsmcs.rds")
propdmgsmcs <- readRDS("propdmgsmcs.rds")

#### my Barplot functions  


myUSABarplot <- function(propdmgms=propdmgms, propdmgs=propdmgs, mtitle = "Property Damages (Economic) in USA due to Top 4 Weather Causes during year - Month by Month in $"){

 pValuesm = matrix(rep(0,4*12),12,4)
 l = c("a","b","c","d")
 for(k in 1:4){
  
	  c2 = propdmgms$EventTypes == propdmgs$Group.1[k]
	  pValues = propdmgms[((c2)),]$Value
	  pnames = propdmgms[((c2)),]$Month
	  
	  pValues
	  pnames
	  mtitle
	  t1 = data.frame(v=pValues, n=pnames)
	  t2 = data.frame(v=rep(0,12), n=c(1:12))
	  t2$v[t2$n %in% t1$n] = t1$v

	  pValues1 = t2$v
	  pnames1 = t2$n
	  pValuesm[,k]=pValues1
	  l[k] <- as.character(propdmgs$Group.1[k])
  
  }

  tpValuesm = t(pValuesm)
  barplot(tpValuesm,names.arg = pnames1,las=1, main=mtitle, xlab = "MONTH", cex.names=1.5, cex.lab = 1.5, cex.axis = 1.2, cex.main=1.5, col = c("RED","BLUE","GREEN", "YELLOW"), beside = TRUE, legend=l)

}


myStateBarplot <- function(propdmgsms, propdmgss, state = "SC", mtitle = "Property Damages (Economic) in SC due to: Top 4 Weather Causes by Month in $" ){
   
  pValuesm = matrix(rep(0,4*12),12,4)
  l = c("a","b","c","d")
  for(k in 1:4){
	  c1 = propdmgsms$State == state
	  c2 = propdmgsms$EventTypes == propdmgss[(propdmgss$State == state),]$EventTypes[k]
	  pValues = propdmgsms[((c1) & (c2)),]$Value
	  pnames = propdmgsms[((c1) & (c2)),]$Month
	  
	  pValues
	  pnames
	  mtitle
	  t1 = data.frame(v=pValues, n=pnames)
	  t2 = data.frame(v=rep(0,12), n=c(1:12))
	  t2$v[t2$n %in% t1$n] = t1$v

	  pValues1 = t2$v
	  pnames1 = t2$n
	  pValuesm[,k]=pValues1
	  l[k] <- as.character(propdmgss[(propdmgss$State == state),]$EventTypes[k])
  
  }

  tpValuesm = t(pValuesm)
  barplot(tpValuesm,names.arg = pnames1,las=1,main=mtitle, xlab = "MONTH", cex.names=1.5, cex.lab = 1.5, cex.axis = 1.2, cex.main=1.5, col = c("RED","BLUE","GREEN", "YELLOW"), beside = TRUE, legend=l)
 
}



myCountyBarplot <- function(propdmgsmcs, propdmgssc, state = "SC", county = "BERKELEY", mtitle="Property Damages (Economic) in SC County BERKELEY due to: Top 4 Weather Causes by Month in $"){
  pValuesm = matrix(rep(0,4*12),12,4)
  l = c("a","b","c","d")
  for(k in 1:4){
	  c1 = propdmgsmcs$State == state
	  c2 = propdmgsmcs$County == county
	  c3 = propdmgsmcs$EventTypes == propdmgssc[((propdmgssc$Group.3 == county)),]$Group.1[k]

	  pValues = propdmgsmcs[((c1) & (c2) & (c3)),]$Value
	  pnames = propdmgsmcs[((c1) & (c2) & (c3)),]$Month
	  
	  pValues
	  pnames
	  mtitle
	  t1 = data.frame(v=pValues, n=pnames)
	  t2 = data.frame(v=rep(0,12), n=c(1:12))
	  t2$v[t2$n %in% t1$n] = t1$v

	  pValues1 = t2$v
	  pnames1 = t2$n
	  pValuesm[,k]=pValues1
	  l[k] <- as.character(propdmgssc[(propdmgssc$Group.3 == county),]$Group.1[k])
  
  }

  tpValuesm = t(pValuesm)
  barplot(tpValuesm,names.arg = pnames1,las=1, main=mtitle, xlab = "MONTH", cex.names=1.5, cex.lab = 1.5, cex.axis = 1.2, cex.main=1.5, col = c("RED","BLUE","GREEN", "YELLOW"), beside = TRUE, legend=l)
}

CreateCountyList <- function(state="SC"){
  c1 = unique(fatsmcs[fatsmcs$State==state,]$County)
  cn=grep("[0-9,]",c1)
  CountyList <- c1[-cn]
  CountyList
}




shinyServer(function(input, output) {
  
  # Drop-down selection box for which data set
  output$choose_State <- renderUI({
    selectInput("State", "State",  gsub("([A-Z]*)", "\\1", unique(fatsms$State)), selected = "MD")
  })
  
  # Drop-down selection box for which data set
  
  output$choose_County <- renderUI({
    selectInput("County", "County",  gsub("([A-Z]*)", "\\1", CreateCountyList(state=input$State)), selected = "BALTIMORE" )
  })
  

  output$plot <- renderPlot({
    
    m <- rbind(c(5,5,5,1,1,2,2,11,11),c(5,5,5,1,1,2,2,11,11),c(6,6,6,1,1,2,2,10,10),c(6,6,6,10,10,10,10,10,10),
               c(7,7,7,3,3,4,4,12,12),c(7,7,7,3,3,4,4,12,12),c(8,8,8,3,3,4,4,9,9),c(8,8,8,9,9,9,9,9,9))
	layout(m)
	par(mar = c(5, 7, 6, 1), oma = c(0, 0, 3, 0))

	barplot(fatu[1:10,]$x,names.arg = fatu[1:10,]$Group.1,las=2,main="Total Fatalities (Health)",cex.names=1.2)
	barplot(inju[1:10,]$x,names.arg = inju[1:10,]$Group.1,las=2,main="Total Injuries (Health)",cex.names=1.2)
	barplot(cropdmgu[1:10,]$x,names.arg = cropdmgu[1:10,]$Group.1,las=2,main="Total Crop Damage (Economic)  in Dollars",cex.names=1.2)
	barplot(propdmgu[1:10,]$x,names.arg = propdmgu[1:10,]$Group.1,las=2,main="TotalProperty Damage (Economic)  in Dollars",cex.names=1.2)
	mtext("Top 10 Weather Causes for Health and Economic Damages in Entire United States during the years in the data",outer=TRUE)

	myUSABarplot(propdmgms=fatms, propdmgs=fatu, mtitle = "Total Fatalities in USA month by month \n due to Top 4 Weather Causes for all the years in the data")
	myUSABarplot(propdmgms=injms, propdmgs=inju, mtitle = "Total Injuries in USA month by month \n due to Top 4 Weather Causes for all the years in the data")
	myUSABarplot(propdmgms=cropdmgms, propdmgs=cropdmgu, mtitle = "Total Crop Damages (Economic) in USA month by month \n due to Top 4 Weather Causes for all the years in the data in Dollars")
	myUSABarplot(propdmgms=propdmgms, propdmgs=propdmgu, mtitle = "Total Property Damages (Economic) in USA month by month \n due to Top 4 Weather Causes for all the years in the data in Dollars")

  }, width = 1584, height = 720, res = 72)
  
  output$plot2 <- renderPlot({
    
    c1 = unique(fatsmcs[fatsmcs$State==input$State,]$County)
    cn=grep("[0-9 ]",c1)
    CountyList <- c1[-cn]
    
      m <- rbind(c(5,5,5,1,1,2,2,11,11),c(5,5,5,1,1,2,2,11,11),c(6,6,6,1,1,2,2,10,10),c(6,6,6,10,10,10,10,10,10),
                 c(7,7,7,3,3,4,4,12,12),c(7,7,7,3,3,4,4,12,12),c(8,8,8,3,3,4,4,9,9),c(8,8,8,9,9,9,9,9,9))
    	layout(m)
    	par(mar = c(5, 7, 6, 1), oma = c(0, 0, 3, 0))
    	
    
    	ustate = c(input$State) # change this variable for other state's plots 
    	
    	for(s in ustate[order(ustate)]){
    	    barplot(fatss[fatss$State == s,][1:10,]$Value,names.arg = fatss[fatss$State == s,][1:10,]$EventTypes,las=2,main="Total Fatalities (Health)",cex.names=1.2)
    	    barplot(injss[injss$State == s,][1:10,]$Value,names.arg = injss[injss$State == s,][1:10,]$EventTypes,las=2,main="Total Injuries (Health)",cex.names=1.2)
    	    barplot(cropdmgss[cropdmgss$State == s,][1:10,]$Value,names.arg = cropdmgss[cropdmgss$State == s,][1:10,]$EventTypes,las=2,main="Total Crop Damage (Economic)  in Dollars",cex.names=1.2)
    	    barplot(propdmgss[propdmgss$State == s,][1:10,]$Value,names.arg = propdmgss[propdmgss$State == s,][1:10,]$EventTypes,las=2,main="Total Property Damage (Economic)  in Dollars",cex.names=1.2)
    	    ts = paste("Top 10 Weather Causes for Health and Economic Damages by individual State: ",s)
    	    
    	    mtext(ts,outer=TRUE)
    	}
    	
    	
      ts = paste(ustate[1],": Total State Fatalities by month \n due to Top 4 Weather Causes for all the years in the data")
    	myStateBarplot(propdmgsms=fatsms, propdmgss=fatss, state=ustate[1], mtitle=ts )
      ts = paste(ustate[1],": Total State Injuries by month \n due to Top 4 Weather Causes for all the years in the data")
    	myStateBarplot(propdmgsms=injsms, propdmgss=injss, state=ustate[1], mtitle=ts )
      ts = paste(ustate[1],": Total State Crop Damages (Economic) by month \n due to Top 4 Weather Causes for all the years in the data in Dollars")
    	myStateBarplot(propdmgsms=cropdmgsms, propdmgss=cropdmgss, state=ustate[1], mtitle=ts )
      ts = paste(ustate[1],": Total State Property Damages (Economic) by month \n due to Top 4 Weather Causes for all the years in the data in Dollars")
    	myStateBarplot(propdmgsms=propdmgsms, propdmgss=propdmgss, state=ustate[1], mtitle=ts)
    
      }, width = 1584, height = 720, res = 72)
  
  output$plot3 <- renderPlot({
    
    m <- rbind(c(5,5,5,1,1,2,2,11,11),c(5,5,5,1,1,2,2,11,11),c(6,6,6,1,1,2,2,10,10),c(6,6,6,10,10,10,10,10,10),
               c(7,7,7,3,3,4,4,12,12),c(7,7,7,3,3,4,4,12,12),c(8,8,8,3,3,4,4,9,9),c(8,8,8,9,9,9,9,9,9))
    layout(m)
    par(mar = c(5, 7, 6, 1), oma = c(0, 0, 3, 0))
    
    
    ustateo = input$State # change this variable for other state's plots   
      
    
    for(s in ustateo){

      counties = input$County # change this variable for other counties's plots 
      # ex: counties = c("ANDERSON") in SC with ustateo = c("SC")
      # ex: counties = countiesall - for all counties in the state selected by ustateo.
      for(c in counties){
        
        barplot(fatssc[(fatssc$Group.2 == s) & (fatssc$Group.3 == c),][1:10,]$x,names.arg = fatssc[(fatssc$Group.2 == s) & (fatssc$Group.3 == c),][1:10,]$Group.1,las=2,main="Total Fatalities (Health)",cex.names=1.2)
        barplot(injssc[(injssc$Group.2 == s) & (injssc$Group.3 == c),][1:10,]$x,names.arg = injssc[(injssc$Group.2 == s) & (injssc$Group.3 == c),][1:10,]$Group.1,las=2,main="Total Injuries (Health)",cex.names=1.2)
        barplot(cropdmgssc[(cropdmgssc$Group.2 == s) & (cropdmgssc$Group.3 == c),][1:10,]$x,names.arg = cropdmgssc[(cropdmgssc$Group.2 == s) & (cropdmgssc$Group.3 == c),][1:10,]$Group.1,las=2,main="Total Crop Damage (Economic)  in Dollars",cex.names=1.2)
        barplot(propdmgssc[(propdmgssc$Group.2 == s) & (propdmgssc$Group.3 == c),][1:10,]$x,names.arg = propdmgssc[(propdmgssc$Group.2 == s) & (propdmgssc$Group.3 == c),][1:10,]$Group.1, las=2, main="Total Property Damage (Economic)  in Dollars",cex.names=1.2)
        ts = paste("Top 10 Weather Causes for Health and Economic Damages individual state County or region: ",c, " in State: ",s)
        
        mtext(ts,outer=TRUE)
      }
    }
    
    ts1 = paste(input$State, " (State) : ")
    ts1 = paste(ts1, input$County)
    ts1 = paste(ts1, " (County) : ")
    ts = paste(ts1, " Total Fatalities by month \n due to Top 4 Weather Causes for all the years in the data")
    myCountyBarplot(propdmgsmcs=fatsmcs, propdmgssc=fatssc, state = input$State, county = input$County, mtitle=ts)
    ts = paste(ts1, " Total Injuries by month \n due to Top 4 Weather Causes for all the years in the data")
    myCountyBarplot(propdmgsmcs=injsmcs, propdmgssc=injssc, state = input$State, county = input$County, mtitle=ts)
    ts = paste(ts1, " Total Crop Damages (Economic) by month \n due to Top 4 Weather Causes for all the years in the data in Dollars")
    myCountyBarplot(propdmgsmcs=cropdmgsmcs, propdmgssc=cropdmgssc, state = input$State, county = input$County, mtitle=ts)
    ts = paste(ts1, " Total Property Damages (Economic) by month \n due to Top 4 Weather Causes for all the years in the data in Dollars")
    myCountyBarplot(propdmgsmcs=propdmgsmcs, propdmgssc=propdmgssc, state = input$State, county = input$County, mtitle=ts)
    
  }, width = 1584, height = 720, res = 72)  
})