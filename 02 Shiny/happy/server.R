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
  
  KPI_Low_Max_value <- reactive({input$KPI1})     
  KPI_Medium_Max_value <- reactive({input$KPI2})
  rv <- reactiveValues(alpha = 0.50)
  observeEvent(input$light, { rv$alpha <- 0.50 })
  observeEvent(input$dark, { rv$alpha <- 0.75 })
  
  df1 <- eventReactive(input$clicks1, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                                 "select color, clarity, sum_price, round(sum_carat) as sum_carat, kpi as ratio, 
                                                                                 case
                                                                                 when kpi < "p1" then \\\'03 Low\\\'
                                                                                 when kpi < "p2" then \\\'02 Medium\\\'
                                                                                 else \\\'01 High\\\'
                                                                                 end kpi
                                                                                 from (select color, clarity, 
                                                                                 sum(price) as sum_price, sum(carat) as sum_carat, 
                                                                                 sum(price) / sum(carat) as kpi
                                                                                 from diamonds
                                                                                 group by color, clarity)
                                                                                 order by clarity;"
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_UTEid', PASS='orcl_UTEid', 
                                                                                                   MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value(), p2=KPI_Medium_Max_value()), verbose = TRUE)))
  })
  
  output$distPlot1 <- renderPlot({             
    plot1 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title=isolate(input$title)) +
      labs(x=paste("COLOR"), y=paste("CLARITY")) +
      layer(data=df1(), 
            mapping=aes(x=COLOR, y=CLARITY, label=SUM_PRICE), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=df1(), 
            mapping=aes(x=COLOR, y=CLARITY, fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=rv$alpha), 
            position=position_identity()
      )
    plot1
  }) 
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  # Begin code for Second Tab:
  
  dfchl <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=                                          "select YEAR, DISEASE, sum_COUNT, sum(sum_COUNT)                                OVER (PARTITION BY DISEASE) as window_avg_COUNT
from 
(select YEAR, DISEASE, sum(COUNT) as sum_COUNT
from Infectious_Diseases
where DISEASE = (\'Chlamydia\') and (SEX=(\'Male\') OR SEX=(\'Female\') )
group by YEAR, DISEASE)
order by DISEASE;"
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_hys82', PASS='orcl_hys82', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))

  
  dfMalechl <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select YEAR, DISEASE, sum_COUNT, sum(sum_COUNT) 
OVER (PARTITION BY DISEASE) as window_avg_COUNT
from 
(select YEAR, DISEASE, sum(COUNT) as sum_COUNT
from Infectious_Diseases
where DISEASE = (\'Chlamydia\') and SEX=(\'Male\')
group by YEAR, DISEASE)
order by DISEASE;"
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_hys82', PASS='orcl_hys82', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))

  
  output$distPlotchl <- renderPlot(height=300, width=700, {
    plotchl <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='CHLAMYDIA AVERAGE_COUNT, WINDOW_AVG_COUNT') +
      labs(x=paste("YEAR"), y=paste("COUNT OVER ALL COUNTIES PER YEAR")) +
      layer(data=dfchl, 
            mapping=aes(x=YEAR, y=SUM_COUNT), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(fill="RED"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=dfMalechl, 
            mapping=aes(x=YEAR, y=SUM_COUNT), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(fill="BLUE"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=dfchl, 
            mapping=aes(yintercept = WINDOW_AVG_COUNT/14), 
            geom="hline",
            geom_params=list(colour="BLACK")
      ) +
      layer(data=dfchl, 
            mapping=aes(x=2000, y=round(WINDOW_AVG_COUNT/14), label=round(WINDOW_AVG_COUNT/14)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-.1), 
            position=position_identity()
      )
    plotchl
  })
  
  # Begin code for Third Tab:
  
  df3 <- eventReactive(input$clicks3, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                                 """select region || \\\' \\\' || \\\'Sales\\\' as measure_names, sum(sales) as measure_values from SUPERSTORE_SALES_ORDERS
                                                                                 where country_region = \\\'United States of America\\\'
                                                                                 group by region
                                                                                 union all
                                                                                 select market || \\\' \\\' || \\\'Coffee_Sales\\\' as measure_names, sum(coffee_sales) as measure_values from COFFEE_CHAIN
                                                                                 group by market
                                                                                 order by 1;"""
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_UTEid', PASS='orcl_UTEid', 
                                                                                                   MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
  
  output$distPlot3 <- renderPlot(height=1000, width=2000, {
    plot3 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      #facet_wrap(~CLARITY, ncol=1) +
      labs(title='Blending 2 Data Sources') +
      labs(x=paste("Region Sales"), y=paste("Sum of Sales")) +
      layer(data=df3(), 
            mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="blue"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=df3(), 
            mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES, label=round(MEASURE_VALUES)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-0.5), 
            position=position_identity()
      )
    plot3
  })
  
  # Begin code for Fourth Tab:
  output$map <- renderLeaflet({leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 17) %>% addPopups(-93.65, 42.0285, 'Here is the Department of Statistics, ISU')
  })
  
  # Begin code for Fifth Tab:
  output$table <- renderDataTable({datatable(df1())
  })
})
