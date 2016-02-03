library(shiny)
  
# Define UI for dataset viewer application
pageWithSidebar(    
    
    headerPanel("Air Pollutant Sample Data"),
    
    sidebarPanel(
      textInput("fips", "Fips:", "24510"),
      p("Input 5 digits to search the target city"),      
      
      selectInput("type", "Choose the source type:", choices = c("POINT", "NONPOINT", "ON-ROAD", "NON-ROAD"),"POINT"),
      
      actionButton("goButton", "Process"),
      p("Click the button to update the results."),
      
      p(""),
      p('fips: A five-digit number (represented as a string) indicating the U.S. county. Reference: http://www.epa.gov/enviro/html/codes/state.html'), 
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