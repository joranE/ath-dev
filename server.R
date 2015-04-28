library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
#library(Cairo)
#options(shiny.usecairo = TRUE)
source("helpers.R")
source("load_data.R")

shinyServer(function(input, output) {

  elite <- reactive({
    comparison_grp(top = input$top,
                   times = input$times,
                   events = input$events)
  })
  
  observe({elite_data <- elite()})
  
  ath <- reactive({
    ath_data(nms = input$nameInput,by_tech = input$by_tech)
  })
  observe({ath_data <- ath()})
  
  #Output
  output$comp_grp_n <- renderText({
      paste("Comparison group currently contains <b>",
            elite()$n_gender[1],"</b>men and <b>",elite()$n_gender[2],"</b>women.")
  })
  
  output$plot1 <- renderPlot({
    if (is.null(ath())) NULL
    else ath_dev_fis(ath()$ath_sum_fis,elite()$elite_sum_fis,input$by_tech)
  })
  
  output$plot2 <- renderPlot({
    if (is.null(ath())) NULL
    else ath_dev_start(ath()$ath_sum_start,elite()$elite_sum_start,"quality")
  })
  
  output$plot3 <- renderPlot({
    if (is.null(ath())) NULL
    else ath_dev_start(ath()$ath_sum_start,elite()$elite_sum_start,"starts")
  })

})
