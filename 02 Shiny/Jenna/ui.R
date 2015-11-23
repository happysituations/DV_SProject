#ui.R

require(shiny)
require(shinydashboard)
require(leaflet)

navbarPage(
  title = "Project 6",
  tabPanel(title = "Crosstab",
           sidebarPanel(
             sliderInput("KPI1", "KPI_Very_Low_Value:", 
                         min = -10, max = 0,  value = 0),
             sliderInput("KPI2", "KPI_Low_Value:", 
                         min = 0, max = 10,  value = 5),
             sliderInput("KP32", "KPI_Medium_Value:", 
                         min = 10, max = 100,  value = 50),
             textInput(inputId = "title", 
                       label = "Crosstab Title",
                       value = "Number of Diseases per Sex per County"),
             actionButton(inputId = "clicks1",  label = "Click me")
           ),
           
           mainPanel(plotOutput("distPlot1")
           )
  )
)

