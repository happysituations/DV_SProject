# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)

shinyServer(function(input, output) {
  diseases <- eventReactive(input$clicks1, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select * from Infectious_Diseases;"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jnw653', PASS='orcl_jnw653', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
  
  #KPI_Very_Low_Value <- reactive({input$KPI1})     
  #KPI_Low_Value <- reactive({input$KPI2})
  #KPI_Medium_Value <- reactive({input$KPI3})
  
df1 <- diseases() %>% group_by(SEX, COUNTY) %>% summarize(avg_count = mean(COUNT)) %>% mutate(kpi = avg_count)# %>% mutate(kpi = ifelse(kpi <= KPI_Very_Low_Value, 'Very Low', ifelse(kpi <= KPI_Low_Value, 'Low', ifelse(kpi <= KPI_Medium_Value, 'Medium', 'High'))))
  
  
  output$distPlot1 <- renderPlot({             
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title='Number of Diseases per Sex per County') +
      labs(x=paste("SEX"), y=paste("County")) +
      layer(data=df1(), 
            mapping=aes(x=SEX, y=COUNTY, label=round(avg_count, 2)),
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", size = 3), 
            position=position_identity()
      ) +
      layer(data=df1(), 
            mapping=aes(x=SEX, y=COUNTY, label=round(avg_count, 2)),
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", vjust=4, hjust=-1, size = 3), 
            position=position_identity()
      ) +
      layer(data=df1(), 
            mapping=aes(x=SEX, y=COUNTY, fill=kpi), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      ) + theme(axis.text.y = element_text(face = "plain", size = 8))
      
    plot
  })
})

  

  
  
