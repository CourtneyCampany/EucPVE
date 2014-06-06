#this scrupt defines the interface(sliders, etc)


library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Effects of belowground space limitation on performance of Eucalyptus seedlings: 
              barrier sensing or nutrient limitation?"),
  
  # Sidebar with controls to select the variables to plot against Photosynthesis
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput("variable", "Variable:",
                list("Runs" = "R",
                     "Hits" = "H", 
                     "Home Runs" = "HR", 
                     "Doubles" = "X2B",
                     "Triples" = "X3B",         
                     "Walks" = "BB",
                     "Strikeouts" = "SO",
                     "Stolen Bases" = "SB",
                     "Errors" = "E")),
    sliderInput("range", "Range:",
                min = 1901, max = 2012, format="###",
                value = c(1901, 2012), step = 1),
    sliderInput("decimal", "Loess Smoothing Fraction:", 
                min = 0.05, max = 0.95, value = 0.2, step= 0.05)
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h3(textOutput("caption")),
    
    plotOutput("mpgPlot")
  )
))



#start with Photo synthesis (Asat and Amax)
#then use the covariates as the list (N, tnc, lma, )