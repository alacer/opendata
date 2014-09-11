library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
#library(lattice)
library(dplyr)
library(rCharts)

# increasing max Shiny file upload
options(shiny.maxRequestSize=100*1024^2)

timedims <- c(
    "Please Select:", 
    "Month"="MonthofYear", 
    "Week of Year"="WeekofYear", 
    "Day of Month"="DayofMonth", 
    "Day of Week"="DayofWeek", 
    "Hour of Day"="HourofDay",
    "Hour of Week"="HourofWeek"
  )

shinyServer(function(input, output, session) {

  dataInput <- reactive({
    inFile <- input$file
      if (is.null(inFile))
        return(NULL)
    
    read.csv(inFile$datapath, stringsAsFactors=TRUE)
  })
  
  cleandata <- reactive({
    #browser()
    if(is.null(dataInput())){
      return(NULL)
    } 
   # if(!is.null(dataInput())){
    alldata <- dataInput()[!is.na(alldata$LAT) || !is.na(alldata$LONG),]

    alldata$TR <- as.character(alldata$TR)
    alldata$DateTime <- as.POSIXct(alldata$TR, format="%m/%d/%Y %H:%M")
    if(is.na(alldata$DateTime[1])==TRUE){
      alldata$DateTime <- as.POSIXct(alldata$TR, format="%m/%d/%Y")      
    }
    if(is.na(alldata$DateTime[1])==TRUE){
      alldata$DateTime <- as.POSIXct(alldata$TR, format="%m-%d-%Y")      
    }
    if(is.na(alldata$DateTime[1])==TRUE){
      alldata$DateTime <- as.POSIXct(alldata$TR, format="%B %d, %Y")      
    }         
    alldata$MonthofYear <- as.character(alldata$DateTime, format="%b")
    alldata$MonthofYear <- factor(alldata$MonthofYear, 
      levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    alldata$MonthofYear <- alldata$MonthofYear[,drop=TRUE]
    alldata$WeekofYear <- as.numeric(as.character(alldata$DateTime, format="%W"))
    alldata$DayofMonth <- as.numeric(as.character(alldata$DateTime, format="%d"))
    alldata$DayofWeek <- as.character(alldata$DateTime, format="%a")
    alldata$DayofWeek <- factor(alldata$DayofWeek,
      levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
    alldata$DayofWeek <- alldata$DayofWeek[,drop=TRUE]
    alldata$HourofDay <-  as.numeric(as.character(alldata$DateTime, format="%H"))
    alldata$HourofWeek <- paste(alldata$HourofDay, alldata$DayofWeek, sep="-")
    alldata$HourofWeek <- factor(alldata$HourofWeek, 
         levels=c(paste(c(0:23),"Sun", sep="-"), paste(c(0:23),"Mon", sep="-"), paste(c(0:23),"Tue", sep="-"), 
          paste(c(0:23),"Wed", sep="-"), paste(c(0:23),"Thu", sep="-"), paste(c(0:23),"Fri", sep="-"), 
          paste(c(0:23),"Sat", sep="-"))
      )
    alldata$HourofWeek <- alldata$HourofWeek[,drop=TRUE]
      
    return(alldata)
  })

  observe({
    if(!is.null(cleandata())){
      latdist <- abs(max(cleandata()$LAT)-min(cleandata()$LAT))*.05
      longdist <- abs(max(cleandata()$LONG)-min(cleandata()$LONG))*.05
      map$fitBounds(min(cleandata()$LAT)-latdist, min(cleandata()$LONG)-longdist,
        max(cleandata()$LAT)+latdist, max(cleandata()$LONG)+longdist)
    }
  })


  
  ## Interactive Map ###########################################

###############
# Points need to be created reactively to input of data and selection of field and time values  

  # Create the map
  map <- createLeafletMap(session, "map")

  output$eval <- renderUI({
    if(is.null(cleandata())){
      evalvars <- c("(Select File)")
    }
    else{
      evalvars <- names(cleandata()[sapply(cleandata(), function(x) length(levels(x))!=length(x) & length(levels(x))>0 & length(levels(x))<300)])
      evalvars <- evalvars[!evalvars%in%c("DayofWeek", "MonthofYear", "HourofWeek")]    
     }
    selectInput('evalselect', 'Evaluate:', evalvars, multiple=FALSE, selectize=FALSE)
  })

  output$evalfilter <- renderUI({
    if(is.null(input$evalselect) || input$evalselect=='Select File'){
      evalfiltervars <- c("(Select Evaluation)")
    }
    else{
      evalfiltervars <- c("All", levels(cleandata()[[input$evalselect]]))    
    }
    selectInput('evalfilterselect', 'Filter by:', evalfiltervars, multiple=TRUE, selectize=FALSE)
  })

  output$time <- renderUI({
    if(is.null(cleandata())){
      timevars <- c("(Select File)")
    }
    else{
      timevars <- timedims[which(timedims%in%names(cleandata()))]
    }
    selectInput('timeselect', 'Time Period:', timevars, multiple=FALSE, selectize=FALSE)
  }) 

  output$timefilter <- renderUI({
    if(is.null(cleandata())){
      timefiltervars <- c("(Select File)")
    }
    if(is.null(input$timeselect) || input$timeselect=='Please Select:'){
      timefiltervars <- c("(Select Time Increment)")
    }
    else{
      timefiltervars <- c("All", levels(as.factor(cleandata()[[input$timeselect]])))
    }
    selectInput('timefilterselect', 'Time Periods:', timefiltervars, multiple=TRUE, selectize=FALSE)
  }) 

  # A reactive expression that returns the set of points that are
  # in bounds right now

  pointInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(data[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    subset(cleandata(),
      LAT >= latRng[1] & LAT <= latRng[2] &
        LONG >= lngRng[1] & LONG <= lngRng[2])
  })
  
  
  pointInSubset <- reactive({

    if(nrow(pointInBounds())>0){
      if(is.null(input$timefilterselect) || input$timefilterselect=="All"){
        datatime <- pointInBounds()
      }
      else if(!is.null(input$timefilterselect) && input$timefilterselect!="All"){
        datatime <- pointInBounds()[which(pointInBounds()[[input$timeselect]]%in%input$timefilterselect),]
      }

      if(is.null(input$evalfilterselect) || input$evalfilterselect=="All"){
        datasubset <- datatime
      }

      else if(!is.null(input$evalfilterselect) && input$evalfilterselect!="All"){
        datasubset <- datatime[which(datatime[[input$evalselect]]%in%input$evalfilterselect),]
      }
    }
    else{
      datasubset <- NULL
    }
    return(datasubset)
  })

  # session$onFlushed is necessary to work around a bug in the Shiny/Leaflet
  # integration; without it, the addCircle commands arrive in the browser
  # before the map is created.
  session$onFlushed(once=TRUE, function() {
    paintObs <- observe({
      if(!is.null(cleandata())){
        if(nrow(pointInBounds())>0){
      if(nrow(pointInSubset())>0){
     # data <- pointInSubset()
      colorBy <- "darkgreen"
      scalefactor <- c(10000)
    
       map$clearShapes()

          map$addCircle(
            pointInSubset()$LAT, pointInSubset()$LONG,
                scalefactor / max(5, input$map_zoom)^2,
             pointInSubset()[,1],
            list(stroke=FALSE, fill=TRUE, fillOpacity=0.6),
            list(color = colorBy)
          )
      }
    }
  }
     })
    
    # TIL this is necessary in order to prevent the observer from
    # attempting to write to the websocket after the session is gone.
    session$onSessionEnded(paintObs$suspend)
  })
  
################################
  showPointPopup <- function(id, lat, lng) {
    popupdata <- pointInSubset()
    selectedPoint <- popupdata[popupdata[,1]==id,]
    content <- as.character(tagList(
      tags$h5(selectedPoint$Full.Address),
      tags$h5(paste(input$evalselect,":", sep=""), selectedPoint[[input$evalselect]]),
      tags$h5("Time:", selectedPoint$TR)
      ))
    map$showPopup(lat, lng, content, id)
  }
###################################
  # When map is clicked, show a popup with city info
  clickObs <- observe({
    map$clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showPointPopup(event$id, event$lat, event$lng)
    })
  })
  
  session$onSessionEnded(clickObs$suspend)
  
  
  ## Data Explorer ###########################################
  
  # observe({
  #   banks <- if (is.null(input$countries)) character(0) else {
  #     filter(cleantable, Point %in% input$countries) %.%
  #       `$`('Bank') %.%
  #       unique() %.%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$banks[input$banks %in% banks])
  #   updateSelectInput(session, "banks", choices = banks,
  #     selected = stillSelected)
  # })
  
  # observe({
  #   if (is.null(input$goto))
  #     return()
  #   isolate({
  #     map$clearPopups()
  #     dist <- 10
  #     point <- input$goto$id
  #     lat <- input$goto$lat
  #     lng <- input$goto$long
  #     #showPointPopup(point, lat, lng)
  #     map$fitBounds(lat - dist, lng - dist,
  #       lat + dist, lng + dist)
  #   })
  # })

  plotdatar <- reactive({
  #  if(input$evalselect!="(Select File)" & input$timeselect!="(Select File)"){
   if(!is.null(pointInSubset()) && nrow(pointInSubset())>0){

    preaggdata <- pointInSubset()

    preaggdata$count <- 1
    aggdata <- aggregate(preaggdata$count, 
      list(preaggdata[[input$evalselect]], 
        preaggdata[[input$timeselect]]), 
        sum
    )
    return(aggdata)
   }
  })


  output$plot1 <- renderChart({
    if (is.null(cleandata()) || nrow(cleandata())==0 || 
        is.null(plotdatar()) || nrow(plotdatar())==0){
      nodata <- data.frame(0,0)
      names(nodata) <- c("x", "y")
      h1 <- hPlot(x = 'x', y = 'y', data=nodata,
        type = "scatter", color="white", title="No Data to Display")
      h1$addParams(dom="plot1")
      h1$chart(height=200, width=500)
      h1$xAxis(title=list(text=""))
      h1$yAxis(title=list(text=""))
      h1$plotOptions(scatter=list(visible=FALSE))
    }
    else{
    plotdata <- plotdatar()
    h1 <- hPlot(x= 'Group.2', y='x', groups='Group.1', type='column', data=plotdata)
    #h1$param(width=500)

    h1$yAxis(title=list(text="Number of Cases"))
    h1$addParams(dom='plot1')
    h1$chart(width=500, height=200)
    h1$legend(enabled=TRUE, margin=20)
    if(input$timeselect=="HourofWeek"){
      h1$xAxis(categories = levels(plotdata$Group.2), title=list(text=""), labels=list(rotation=-90, align='right'), tickInterval=6)
    }
    else{
      h1$xAxis(categories = levels(plotdata$Group.2), title=list(text=""))  
    }    
    # averages <- aggregate(plotdata$x, list(plotdata$Group.2), mean, na.rm=TRUE)
    # h1$series(data=list(averages$x), type="line")

    }
    return(h1)
  })

  predictiondatar <- reactive({
 #   if(input$evalselect!="(Select File)" & input$timeselect!="(Select File)"){
    if(!is.null(plotdatar()) && nrow(plotdatar())>0){
      tmp <- aggregate(plotdatar()$x, list(plotdatar()$Group.2), mean, na.rm=TRUE)
      tmp$x <- round(tmp$x, 2)
      return(tmp)
    }
  }) 

  output$plot2 <- renderChart({
    if (is.null(cleandata()) || nrow(cleandata())==0 || 
        is.null(predictiondatar()) || nrow(predictiondatar())==0){
      nodata <- data.frame(0,0)
      names(nodata) <- c("x", "y")
      h2 <- hPlot(x = 'x', y = 'y', data=nodata,
        type = "scatter", color="white", title="No Data to Display")
      h2$addParams(dom="plot2", width=500)
      h2$xAxis(title=list(text=""))
      h2$yAxis(title=list(text=""))
      h2$chart(width=500, height=200)
      h2$plotOptions(scatter=list(visible=FALSE))
    }
    else{    
      predictiondata <- predictiondatar()
      h2 <- hPlot(x='Group.1', y='x', type='line', data=predictiondata)
      h2$addParams(dom="plot2")
      h2$chart(width=500, height=200)
      h2$yAxis(title=list(text="Average Number of Cases"))
      h2$legend(enabled=FALSE)
      if(input$timeselect=="HourofWeek"){
      h2$xAxis(categories = levels(predictiondata$Group.1), title=list(text=""), labels=list(rotation=-90, align='right'), tickInterval=6)
      }
      else{
      h2$xAxis(categories = levels(predictiondata$Group.1), title=list(text="")) 
      }
      }
    return(h2)  
  })

 
  
  output$outtable <- renderDataTable({

    cleandata()
     #%.%
      # filter(
      #   is.null(input$countries) | Point %in% input$countries,
      #   is.null(input$banks) | Bank %in% input$banks
      # ) #%.%

      # mutate(Action = paste('<a class="go-map" href="" data-lat="', LAT, '" data-long="', LONG, '" data-point="', 
      #   Point, '"><i class="fa fa-crosshairs"></i></a>', sep="")) 

  })




})
