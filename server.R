library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
source("helpers.R")
source("load_data.R")

shinyServer(function(input, output) {

  elite <- reactive({
    comparison_grp(top = input$top,
                   times = input$times,
                   events = input$events)
  })
  
  elite_sum <- reactive({
    elite() %>%
      collect() %>%
      filter(age >= 17 & age <= 35) %>%
      group_by(gender,type,age) %>% 
      summarise(lower = quantile(fispoints,probs = 0.25,na.rm = TRUE),
                mid = median(fispoints,na.rm = TRUE),
                upper = quantile(fispoints,probs = 0.75,na.rm = TRUE),
                n = n())})
  
  output$comp_grp_n <- renderText({
    n_gender <- elite() %>% 
      collect() %>%
      group_by(gender) %>%
      summarise(n = n_distinct(fisid)) %>%
      magrittr::extract2("n")
    paste("Comparison group currently contains <b>",
          n_gender[1],"</b>men and <b>",n_gender[2],"</b>women.")
  })
  
  output$plot1 <- renderPlot({
    ath_dev(input$nameInput,input$by_tech,elite_sum())
  })
  
  output$plot2 <- renderPlot({
    ath_dev1(input$nameInput,"quality",elite())
  })
  
  output$plot3 <- renderPlot({
    ath_dev1(input$nameInput,"starts",elite())
  })

})
