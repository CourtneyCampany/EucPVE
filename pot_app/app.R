
library(shiny)
library(doBy)
library(RColorBrewer)

#simple functions-------------------------------------------------------------
datevol_func <- function(x){
  x$volume <- as.factor(x$volume)
  x$Date <- as.Date(x$Date)
  return(x)
}

se <- function(x) sd(x)/sqrt(length(x))

#read and format data----------------------------------------------------------------------

height <- read.csv("height.csv")
diameter <- read.csv("diameter.csv")

#height treatment means
height_agg <- summaryBy(height ~ Date + volume , data = height, FUN = c(mean,se))
  height_agg<- datevol_func(height_agg)

#diameter treatment means
diam_agg <- summaryBy(diameter ~ Date + volume , data = diameter, FUN = c(mean,se))
  diam_agg<- datevol_func(diam_agg)

#leaf area interpolated
leafarea_time <- read.csv("cumulative_leaf_area.csv")
  leafarea_time <-datevol_func (leafarea_time)
  
#plot bits  
  startday <- as.Date(strptime("01-01-2013", format = "%m-%d-%Y", tz=""))
  xlim1 <- as.Date(strptime("01-05-2013", format = "%m-%d-%Y", tz=""))
  xlim2 <- as.Date(strptime("06-01-2013", format = "%m-%d-%Y", tz=""))
  xAT <- seq.Date(startday, by="month", length=6,format = "%m-%d-%Y")
  xlimdays <- c(xlim1, xlim2)  
  pchs <- c(rep(16,6),17)
  
  #colors
  gradient <- colorRampPalette(c("red", "blue"))
  palette(gradient(7))
  cols <- palette(gradient(7))
  
#build shiny-------------------------------------------------------------------------------  

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(""),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(position="right",
                 sidebarPanel(width=3,
                   style = "border-color: #cc2904; background-color: white; border-style:dotted;border-width:thick",
                   checkboxGroupInput("whichsize", "Pick a Volume:",c("5" = "5", 
                    "10" = "10", "15" = "15", "20"="20", "25"="25","35"="35", "Free"="1000")
                    ,selected="1000")
                 ),
      
      # Show a plot of the generated distribution
      mainPanel(width=9,
         plotOutput("plot1", height="800px")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  height_ss<- reactive({subset(height_agg, volume %in% input$whichsize)
  })
  
  diam_ss<- reactive({subset(diam_agg, volume %in% input$whichsize)
  })
  
  la_ss<- reactive({subset(leafarea_time, volume %in% input$whichsize)
  })

   
   output$plot1 <- renderPlot({
     
     
     par(cex.axis=2, cex.lab=2.5,las=1,mgp=c(4,1,0),mfrow=c(3,1),  
         omi=c(.5,0,0.1,0.1)) 
     
     par(mar=c(0,7,2,2))
     plot(height.mean ~ Date, data=height_agg,type='n',ylab="Height  (cm)",  
          ylim=c(20,135), axes=FALSE, xlab="", xlim=xlimdays)  
     axis.Date(1, at=xAT, labels=FALSE) 
     axis(2, labels=TRUE, at=c(0,20,40,60,80,100,120))  
     
     arrows(height_ss()[[1]], height_ss()[[3]], height_ss()[[1]], 
            height_ss()[[3]]+height_ss()[[4]], 
            angle=90,col = height_ss()[[2]],length=0.03, cex=2.5)
   
     arrows(height_ss()[[1]], height_ss()[[3]], height_ss()[[1]],
            height_ss()[[3]]-height_ss()[[4]], 
            angle=90, col = height_ss()[[2]],length=0.03, cex=2.5)

     points(height_ss()[[3]] ~ height_ss()[[1]],pch=pchs[height_ss()[[2]]], cex=2.5, 
            col = height_ss()[[2]])      
     box()
     #diameter
     par(mar=c(0,7,0,2))
     plot(diameter.mean ~ Date, data=diam_agg, type='n',ylab="Diameter  (mm)",
          ylim=c(0,17), axes = FALSE, xlab="",xlim=xlimdays)
     axis.Date(1, at=xAT, labels=FALSE)  
     axis(2)     
     
     arrows(diam_ss()[[1]], diam_ss()[[3]], diam_ss()[[1]],
            diam_ss()[[3]]+diam_ss()[[4]], angle=90, col = diam_ss()[[2]],
            length=0.03, cex=2.5)
     arrows(diam_ss()[[1]], diam_ss()[[3]], diam_ss()[[1]],
            diam_ss()[[3]]-diam_ss()[[4]], angle=90, col = diam_ss()[[2]],
            length=0.03, cex=2.5)
     
     points(diam_ss()[[3]] ~ diam_ss()[[1]],pch=pchs[diam_ss()[[2]]], cex=2.5, 
            col = diam_ss()[[2]])
     box()
     
     par(mar=c(2,7,0,2))
     plot(canopysqm.mean ~ Date, data=leafarea_time, axes=FALSE,xlab="", ylab=expression(Leaf~Area~~(m^2)),
          type='n', ylim=c(0,.7),xlim=xlimdays)
     box()
     axis(2, labels=TRUE, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6))  
     axis.Date(1, at=xAT, label=TRUE, format="%b") 
     
     arrows(la_ss()[[1]], la_ss()[[3]], la_ss()[[1]],
            la_ss()[[3]]+la_ss()[[4]], angle=90, col = la_ss()[[2]],
            length=0.03, cex=2.5)
   
     arrows(la_ss()[[1]], la_ss()[[3]], la_ss()[[1]],
            la_ss()[[3]]-la_ss()[[4]], angle=90, col = la_ss()[[2]],
            length=0.03, cex=2.5)
     
     points(la_ss()[[3]] ~ la_ss()[[1]],pch=pchs[la_ss()[[2]]], cex=2.5, 
            col = la_ss()[[2]])
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

