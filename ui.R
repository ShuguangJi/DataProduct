shinyUI(pageWithSidebar(
  headerPanel('Fuel Economy App'),
  sidebarPanel(
    actionButton("go", "Forcast"),
    selectInput('timescale', 'Time scale', c('1', '5')),
    sliderInput("range", "Year", min = 2015, max = 2030, value = c(2015,2030), sep=""),
    checkboxGroupInput('var', 'Fuel', choices = c('Gas','Diesel','E85','Electricity','CNG','Propane'), selected = 'Gas')
  ),
  
  
  tabsetPanel(
    tabPanel("Time Series Plot", plotOutput("plot1")), 
    tabPanel("Density Plot", plotOutput("plot2")))
   #tabPanel("Summary Table", dataTableOutput('table')))
 
  
   #tabPanel(,
           #fluidRow(
            # column(8, plotOutput("plot1")),
            # column(12, plotOutput("plot2"))
           #))
  #mainPanel(
    #plotOutput('plot1')
  #),
  
  #fluidRow(
    #column(8, plotOutput("plot1")),
   # column(12, dataTableOutput('table'))
  #)
 ))