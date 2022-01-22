#Loading libraries
library(shiny)

#Loading other used scripts

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Titanic"),
  
  # The inout-fields 
  sidebarLayout(
    sidebarPanel(
      radioButtons("pclassIn", "Passenger's class:",
                   c("All" = "all",
                     "First class" = "first",
                     "Secound class" = "secound",
                     "Third class" = "third")),
      selectInput("sexIn", "Passenger's sex:",
                   c("all" = "all",
                     "female" = "female",
                     "male" = "male")),
      selectInput("ageIn",
                   "Passenger's age",
                   c("All" = "all",
                     "Children" = "children",
                     "Adults" = "adults"))
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("pieChart")
    )
  )
))
