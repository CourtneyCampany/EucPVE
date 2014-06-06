
#R code to due the work in the shiny app


library(shiny)
library(Lahman)
library(ggplot2)

# Define server logic required to plot various variables against year
shinyServer(function(input, output) {
  
  # Compute the forumla text in a reactive function since it is 
  # shared by the output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("Variable:", input$variable,
          "relationship with seedling photosynthesis, constrained by container volume"#,
          #input$range[1], "to", input$range[2])#try to make this volume?
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$Asat_plot <- renderPlot({
    Asat_variable <- subset(Nphoto, volume != "1000")
    #Teams.recent$stat.game <- Teams.recent[, input$variable] / Teams.recent[, "G"]
    print(plot(Nphoto, Photo(Nmass, volume)) 
            xlab("Photosynthesis") + ylab("N per unit leaf mass (g/g)")   
    )
  }, width=600, height=500)
})