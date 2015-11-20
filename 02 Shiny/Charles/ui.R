#ui.R
require(shiny)
require(shinydashboard)
require(leaflet)
require(shinydashboard)
require(leaflet)
require(DT)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Scatterplots", tabName = "scatterplots", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "scatterplots",
              textInput(inputId = "title", 
                        label = "Scatterplots Title",
                        value = "Scatterplot of Average Disease Count"),
              actionButton(inputId = "clicks1",  label = "Click me"),
              plotOutput("distPlot1"),
              plotOutput("distPlot2"),
              plotOutput("distPlot3")
      )
    )
  )
)

