
#Libraries
library(class)
library(shiny)
library(colourpicker)
library(tidyverse)
library(dbplyr)
library(caret)
library(e1071)
library(maps)
library(usmap)
library(RANN)
library(shiny)
library(RCurl)
library(shinyFeedback)
library(ggthemes)
library(WDI)
library(ggplot2)
library(olsrr)

ui <- fluidPage(
  tags$figure(
    align = "left",
    tags$img(
      src = "https://i.ibb.co/SJc34j1/houseprediction.jpg",
      width = 150,
      alt = "House Sale Price Prediction"
    ),
    tags$figcaption("")
  ),
  tags$h3(
    tags$strong(tags$span(style="color:darkblue","Regression analysis: Predicting housing prices"))),
  tags$h6(tags$span(style="color:darkblue","Created by Carlos Estevez, email:cestevez@smu.edu")),
  tags$hr(),
  tabsetPanel(id="tabset",
              tabPanel("Data loading",icon = icon("cloud-arrow-up"),
                       fileInput("cflTrFile","Select Kaggle training file",accept = c(".csv", ".tsv")),
                       fileInput("cflTstFile","Select Kaggle testing file",accept = c(".csv", ".tsv")),
                       actionButton("btnLoadData","Load data"),
                       tags$hr(),
                       verbatimTextOutput("cboLoadData")
              ),
              tabPanel("Sale price and House living area",icon = icon("house"),
                       actionButton("btnDoAna1","Perform Analysis"),
                       tags$hr(),
                       tags$h4(tags$strong("Sales vs House Living area")),
                       fluidRow(
                         column(2,"",
                                      checkboxInput("cchName","NAmes",value = TRUE),
                                      checkboxInput("cchEd","Eduards",value = TRUE),
                                      checkboxInput("cchBrk","BrkSide",value = TRUE)),
                         column(6,"",plotOutput("plotDataAna1"))
                       ),
                       tags$h4(tags$strong("Plots before optimization")),
                       tags$hr(),
                       fluidRow(
                         column(6,"",plotOutput("plotDataBeforeOuta1")),
                         column(6,"",plotOutput("plotDataBeforeOuta2"))
                       ),
                       tags$hr(),
                       tags$h4(tags$strong("Plots after optimization")),
                       fluidRow(
                         column(4,"",plotOutput("plotDataAnab1")),
                         column(4,"",plotOutput("plotDataBeforeoutb1")),
                         column(4,"",verbatimTextOutput("resultModel"))
                       )
              )
             
  )
)


