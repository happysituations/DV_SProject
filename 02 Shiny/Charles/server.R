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
   df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from Infectious_Diseases where COUNTY NOT IN (\'California\')"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cmm5627', PASS='orcl_cmm5627', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
   Male_df <- df %>% select(SEX, mean(COUNT), YEAR) %>% filter(SEX == "Male")
   Female_df <- df %>% select(SEX, mean(COUNT), YEAR) %>% filter(SEX == "Female")
   
   output$distPlot1 <- renderPlot({

    plot1 <- df %>% select(SEX, mean(COUNT), YEAR) %>% ggplot(aes(y = mean(COUNT), x = YEAR, color = "TOTAL")) + ggtitle("Average Count of Infectious Diseases by Sex (TOTAL)") + geom_point() + scale_y_continuous() + scale_x_continuous() + coord_cartesian() 
    return(plot1)
  }) 
   
   output$distPlot2 <- renderPlot({
     
     plot2 <- Male_df %>% select(SEX, mean(COUNT), YEAR) %>% ggplot(aes(y = mean(COUNT), x = YEAR, color = "Male")) + ggtitle("Average Count of Infectious Diseases by Sex (MALE)") + geom_point() + scale_y_continuous() + scale_x_continuous() + coord_cartesian()
     return(plot2)
   }) 
   
   output$distPlot3 <- renderPlot({
     
     plot3 <- Female_df <- Female_df %>% select(SEX, mean(COUNT), YEAR) %>% ggplot(aes(y = mean(COUNT), x = YEAR, color = "Female")) + ggtitle("Average Count of Infectious Diseases by Sex (FEMALE)") + geom_point() + scale_y_continuous() + scale_x_continuous() + coord_cartesian() 
     return(plot3)
   }) 
   
})
