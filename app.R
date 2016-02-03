library(shiny)
  
  # Define UI for dataset viewer application
ui<-pageWithSidebar(
    
    
    headerPanel("Air Pollutant"),
    
    sidebarPanel(
      textInput("fips", "Postcode:", "24510"),
      p("Input 5 digits to search city postcode"),
      
      
      selectInput("type", "Choose the source type:", choices = c("POINT", "NONPOINT", "ON-ROAD", "NON-ROAD"),"POINT"),
      
      actionButton("goButton", "Process"),
      p("Click the button to update the results."),
      
      p(""),
      p('fips: A five-digit number (represented as a string) indicating the U.S. country. Reference: http://www.epa.gov/enviro/html/codes/state.html'), 
      p(""),
      p(""),
      p("type: The type of source (point, non-point, on-road, or non-road)")
    ),
    
    mainPanel(
      
      h1('Results'),
      h3("(load data may take some times)"),
      
      h2('Total pollution in 1999 = '), 
      h3(textOutput("result3")),
      
      h2('Total pollution in 2002 = '), 
      h3(textOutput("result4")),
      
      h2('Total pollution in 2005 = '), 
      h3(textOutput("result5")),
      
      plotOutput("plot"),
      
      p('I use the Fine particulate matter (PM2.5) air pollutant sample dataset from Environmental Protection Agency. The dataset is also being used in the Exploratory Data Analysis Course of Coursera.'), 
      p("Because the original data is huge and take a lot of time to load. I've made some pre-processing to increase the speed"),
      
      p("The results will show the total pollutant of year 1999, 2002, and 2005 based on the target city and source type")
    )    
  )

server<-function(input, output) {
  
  nei1 <- read.csv("M9-DevDataProducts/summaryPM25_1999.csv")
  nei2 <- read.csv("M9-DevDataProducts/summaryPM25_2002.csv")
  nei3 <- read.csv("M9-DevDataProducts/summaryPM25_2005.csv") 
  
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
    isolate(nei_array1)
    isolate(f1(as.numeric(input$fips), input$type))
    nei_array1
  })
  
  output$result4 <- renderText({
    input$goButton
    isolate(f2(as.numeric(input$fips), input$type))
    nei_array1
  })
  
  output$result5 <- renderText({
    input$goButton
    isolate(f3(as.numeric(input$fips), input$type))
    nei_array1
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
}
  


shinyApp(ui = ui, server = server)