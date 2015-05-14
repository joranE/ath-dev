library(shiny)
library(shinydashboard)
source("load_names.R")

# Application title
header <- dashboardHeader(title = "Statistical Skier")

# Sidebar with a slider input for number of bins
sidebar <- dashboardSidebar(
  h4("Define comparison group:"),
  numericInput("top","Finished in the top:",3),
  numericInput("times","At least this many times:",5),
  checkboxGroupInput(inputId = "events",
                     label = "In the following events:",
                     choices = c("World Cup"         = "WC",
                                 "Olympics"          = "OWG",
                                 "World Ski Champs"  = "WSC",
                                 "Tour de Ski"       = "TDS"),
                     selected = c("WC","OWG","WSC","TDS")),
  hr(),
  selectizeInput(inputId = "nameInput",
                 label = "Choose an athlete:",
                 choices = c("Start typing..." = "",NAMES),
                 selected = NULL,
                 multiple = TRUE,
                 options = list(maxItems = 4)),
  checkboxInput(inputId = "by_tech",
                label = "Plot by technique:",
                value = FALSE),
  submitButton("Plot Development Comparison"),
  hr(),
  htmlOutput("comp_grp_n")
)

# Show a plot of the generated distribution
body <- dashboardBody(
  fluidRow(
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3")
  )
)


dashboardPage(header,sidebar,body)