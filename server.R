  library(shiny)
  
  server(function(input, output) { 
    
  nei1 <- read.csv("summaryPM25_1999.csv")
  nei2 <- read.csv("summaryPM25_2002.csv")
  nei3 <- read.csv("summaryPM25_2005.csv")   
    
    datatype <- reactive({
      switch(input$dataset,
             "POINT" = "POINT",
             "NONPOINT" = "NONPOINT",
             "ON-ROAD" = "ON-ROAD",
             "NON-ROAD" = "NON-ROAD",)
    })
    
    
    f1 <- function(fip, types) {
      colSums (subset(nei1, nei1[,1] == fip & nei1[,3] == types, select = c("Emissions")), na.rm = T, dims = 1)
    }
    
    f2 <- function(fip, types) {
      colSums (subset(nei2, nei2[,1] == fip & nei2[,3] == types, select = c("Emissions")), na.rm = T, dims = 1)
    }
    
    
    f3 <- function(fip, types) {
      colSums (subset(nei3, nei3[,1] == fip & nei3[,3] == types, select = c("Emissions")), na.rm = T, dims = 1)
    }
    
    
    
    
    output$result3 <- renderText({
      input$goButton
      #isolate(nei_array1)
      isolate(f1(as.numeric(input$fips), input$type))
      #nei_array1
    })
    
    output$result4 <- renderText({
      input$goButton
      isolate(f2(as.numeric(input$fips), input$type))
      #nei_array1
    })
    
    output$result5 <- renderText({
      input$goButton
      isolate(f3(as.numeric(input$fips), input$type))
      #nei_array1
    })
    
    
    output$plot <- renderPlot({
      
      input$goButton
      
      isolate(nei_array <- array(dim=c(3,2)) )
      isolate(nei_array[1,1] <- c("1999") )
      isolate(nei_array[2,1] <- c("2002") )
      isolate(nei_array[3,1] <- c("2005") )
      isolate(nei_array[1,2] <- colSums (subset(nei1, nei1[,1] == as.numeric(input$fips) & nei1[,3] == input$type, select = c("Emissions")), na.rm = T, dims = 1) )
      isolate(nei_array[2,2] <- colSums (subset(nei2, nei2[,1] == as.numeric(input$fips) & nei2[,3] == input$type, select = c("Emissions")), na.rm = T, dims = 1) )
      isolate(nei_array[3,2] <- colSums (subset(nei3, nei3[,1] == as.numeric(input$fips) & nei3[,3] == input$type, select = c("Emissions")), na.rm = T, dims = 1) )
      isolate(plot(nei_array[,1], as.numeric(nei_array[,2]), ylab='Total Emission (ton)', xlab='',type='b') )
      
      
    })
    
    
  })