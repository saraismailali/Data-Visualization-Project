#Loading libraries
# for Shiny
library(shiny)

#for csv
library(readr)
library(data.table)

#Loading other used scripts
source("scripts/pieChart.R")
source("scripts/passenger.R")
source("scripts/filterPassenger.R")
source("scripts/allPass.R")
source("scripts/getSlices.R")

# Loading Data
titanicData <- read.csv2("data/titanic.csv")
setnames(titanicData, old = "ï..pclass", new = "pclass")
countedPassenger <- filterPassenger(titanicData)
lbls <- c("survived", "not survived")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$pieChart <- renderPlot({
    slices <- getSlices(countedPassenger, input)
    pieChart(slices, lbls)
  })
  
})
