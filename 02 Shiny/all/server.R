# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)


shinyServer(function(input, output) {
  
  KPI_Very_Low_Value = 0     
  KPI_Low_Value = 10
  KPI_Medium_Value = 100  
  
  #KPI_Very_Low_Value <- reactive({input$KPI1})     
  #KPI_Low_Value <- reactive({input$KPI2})
  #KPI_Medium_Value <- reactive({input$KPI3})
  
  diseases <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                        "select * from Infectious_Diseases;"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jnw653', PASS='orcl_jnw653', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  
  
  df1 <- diseases %>% group_by(SEX, COUNTY) %>% summarize(avg_count = mean(COUNT)) %>% mutate(kpi = avg_count) %>% mutate(kpi = ifelse(kpi <= KPI_Very_Low_Value, 'Very Low', ifelse(kpi <= KPI_Low_Value, 'Low', ifelse(kpi <= KPI_Medium_Value, 'Medium', 'High'))))
  
  
  output$distPlot1 <- renderPlot({             
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title='Number of Diseases per Sex per County') +
      labs(x=paste("SEX"), y=paste("County")) +
      layer(data=df1, 
            mapping=aes(x=SEX, y=COUNTY, label=round(avg_count, 2)),
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", size = 3), 
            position=position_identity()
      ) +
      layer(data=df1, 
            mapping=aes(x=SEX, y=COUNTY, label=round(avg_count, 2)),
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", vjust=4, hjust=-1, size = 3), 
            position=position_identity()
      ) +
      layer(data=df1, 
            mapping=aes(x=SEX, y=COUNTY, fill=kpi), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      ) + theme(axis.text.y = element_text(face = "plain", size = 8))
    
    plot
  })

  
  
  # Begin code for Second Tab:
  
  dfchl <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select YEAR, DISEASE, sum_COUNT, sum(sum_COUNT)                         OVER (PARTITION BY DISEASE) as window_avg_COUNT
from (select YEAR, DISEASE, sum(COUNT) as sum_COUNT
from Infectious_Diseases
where DISEASE = (\'Chlamydia\') and (SEX=(\'Male\') OR SEX=(\'Female\') )
group by YEAR, DISEASE)
order by DISEASE;"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_hys82', PASS='orcl_hys82', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  
  
  dfMalechl <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select YEAR, DISEASE, sum_COUNT, sum(sum_COUNT) 
OVER (PARTITION BY DISEASE) as window_avg_COUNT
from (select YEAR, DISEASE, sum(COUNT) as sum_COUNT
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
  
  dfgon <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select YEAR, DISEASE, sum_COUNT, sum(sum_COUNT)                         OVER (PARTITION BY DISEASE) as window_avg_COUNT
from (select YEAR, DISEASE, sum(COUNT) as sum_COUNT
                                                     from Infectious_Diseases
                                                     where DISEASE = (\'Gonorrhea\') and (SEX=(\'Male\') OR SEX=(\'Female\') )
                                                     group by YEAR, DISEASE)
                                                     order by DISEASE;"
                                                     ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_hys82', PASS='orcl_hys82', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  
  
  dfMalegon <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select YEAR, DISEASE, sum_COUNT, sum(sum_COUNT) 
                                                         OVER (PARTITION BY DISEASE) as window_avg_COUNT
                                                         from (select YEAR, DISEASE, sum(COUNT) as sum_COUNT
                                                         from Infectious_Diseases
                                                         where DISEASE = (\'Gonorrhea\') and SEX=(\'Male\')
                                                         group by YEAR, DISEASE)
                                                         order by DISEASE;"
                                                         ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_hys82', PASS='orcl_hys82', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  
  
  output$distPlotgon <- renderPlot(height=300, width=700, {
    plotgon <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='GONORRHEA AVERAGE_COUNT, WINDOW_AVG_COUNT') +
      labs(x=paste("YEAR"), y=paste("COUNT OVER ALL COUNTIES PER YEAR")) +
      layer(data=dfgon, 
            mapping=aes(x=YEAR, y=SUM_COUNT), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(fill="RED"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=dfMalegon, 
            mapping=aes(x=YEAR, y=SUM_COUNT), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(fill="BLUE"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=dfgon, 
            mapping=aes(yintercept = WINDOW_AVG_COUNT/14), 
            geom="hline",
            geom_params=list(colour="BLACK")
      ) +
      layer(data=dfgon, 
            mapping=aes(x=2000, y=round(WINDOW_AVG_COUNT/14), label=round(WINDOW_AVG_COUNT/14)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-.1), 
            position=position_identity()
      )
    plotgon
  })

  dfcam <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select YEAR, DISEASE, sum_COUNT, sum(sum_COUNT)                         OVER (PARTITION BY DISEASE) as window_avg_COUNT
from (select YEAR, DISEASE, sum(COUNT) as sum_COUNT
                                                     from Infectious_Diseases
                                                     where DISEASE = (\'Campylobacteriosis\') and (SEX=(\'Male\') OR SEX=(\'Female\') )
                                                     group by YEAR, DISEASE)
                                                     order by DISEASE;"
                                                     ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_hys82', PASS='orcl_hys82', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  
  
  dfMalechl <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select YEAR, DISEASE, sum_COUNT, sum(sum_COUNT) 
                                                         OVER (PARTITION BY DISEASE) as window_avg_COUNT
                                                         from (select YEAR, DISEASE, sum(COUNT) as sum_COUNT
                                                         from Infectious_Diseases
                                                         where DISEASE = (\'Campylobacteriosis\') and SEX=(\'Male\')
                                                         group by YEAR, DISEASE)
                                                         order by DISEASE;"
                                                         ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_hys82', PASS='orcl_hys82', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  
  
  output$distPlotcam <- renderPlot(height=300, width=700, {
    plotcam <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='CAMPYLOBACTERIOSIS AVERAGE_COUNT, WINDOW_AVG_COUNT') +
      labs(x=paste("YEAR"), y=paste("COUNT OVER ALL COUNTIES PER YEAR")) +
      layer(data=dfcam, 
            mapping=aes(x=YEAR, y=SUM_COUNT), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(fill="RED"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=dfMalecam, 
            mapping=aes(x=YEAR, y=SUM_COUNT), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(fill="BLUE"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=dfcam, 
            mapping=aes(yintercept = WINDOW_AVG_COUNT/14), 
            geom="hline",
            geom_params=list(colour="BLACK")
      ) +
      layer(data=dfcam, 
            mapping=aes(x=2000, y=round(WINDOW_AVG_COUNT/14), label=round(WINDOW_AVG_COUNT/14)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-.1), 
            position=position_identity()
      )
    plotcam
  })
  
  
  # Begin code for Third Tab:
  
  df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from Infectious_Diseases where COUNTY NOT IN (\'California\')"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cmm5627', PASS='orcl_cmm5627', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))
  Male_df <- df %>% select(SEX, mean(COUNT), YEAR) %>% filter(SEX == "Male")
  Female_df <- df %>% select(SEX, mean(COUNT), YEAR) %>% filter(SEX == "Female")
  
  output$distPlot2 <- renderPlot({
    
    plot1 <- df %>% select(SEX, mean(COUNT), YEAR) %>% ggplot(aes(y = mean(COUNT), x = YEAR, color = "TOTAL")) + ggtitle("Average Count of Infectious Diseases by Sex (TOTAL)") + geom_point() + scale_y_continuous() + scale_x_continuous() + coord_cartesian() 
    return(plot1)
  }) 
  
  output$distPlot3 <- renderPlot({
    
    plot2 <- Male_df %>% select(SEX, mean(COUNT), YEAR) %>% ggplot(aes(y = mean(COUNT), x = YEAR, color = "Male")) + ggtitle("Average Count of Infectious Diseases by Sex (MALE)") + geom_point() + scale_y_continuous() + scale_x_continuous() + coord_cartesian()
    return(plot2)
  }) 
  
  output$distPlot4 <- renderPlot({
    
    plot3 <- Female_df <- Female_df %>% select(SEX, mean(COUNT), YEAR) %>% ggplot(aes(y = mean(COUNT), x = YEAR, color = "Female")) + ggtitle("Average Count of Infectious Diseases by Sex (FEMALE)") + geom_point() + scale_y_continuous() + scale_x_continuous() + coord_cartesian() 
    return(plot3)
  })
  
})
