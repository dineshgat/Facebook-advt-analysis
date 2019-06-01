library(shiny)
library(shinydashboard)
library(lubridate)
library(stringr)
library(tidyverse)
library(dplyr)

shinyServer(function(input,output)
{
  
  
  output$mytable <-DT::renderDataTable({
    
    data=read_csv("fb_ad_data.csv")
    data=rename(data,campid = xyz_campaign_id, fbid = fb_campaign_id, impr = Impressions,
                conv = Total_Conversion, appConv = Approved_Conversion)
    data
    
  })
  
  output$distPlot <- renderText({
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    input$go
    
    # Use isolate() to avoid dependency on input$obs
    dist <- isolate(input$in1)
    dist
  })
  
  
  
  
})