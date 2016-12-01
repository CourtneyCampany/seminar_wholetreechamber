

library(shiny)
library(doBy)
library(scales)
library(mgcv)

gmes <- read.csv("gmes_wellwatered.csv")
source("functions.R")

suncol <- alpha("#ff7f00",0.85) 
shacol <-  alpha("#377eb8", 0.85) 
lightscol <- alpha("#47a244", 0.85) 
allcols2=c(suncol, shacol, lightscol, "black", "black")
alllab2 <- c("Sun", "Shade-Low Light", "Shade-High Light", "AT", "ET")

###get average by id
gm_agg <- summaryBy(Photo+ Cond + gm_bar ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)
cond_dat <- gm_agg[, c("leaflight", "Photo", "Cond", "temp")]

leafnames <- unique(cond_dat$leaflight)
leafnames2 <- data.frame(leaflight = leafnames, colorleaf = c(lightscol, shacol, suncol))

cond_dat2 <- merge(cond_dat, leafnames2)
  cond_dat2$colorleaf <- as.character(cond_dat2$colorleaf)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   titlePanel(""),
   sidebarLayout(
     ##type selection
     sidebarPanel(
       style = "border-color: #cc2904; background-color: white; border-style:dotted;border-width:medium",
       checkboxGroupInput("whichleaf", "Pick a leaf:",c("Sun" = "sun-high", "Shade-low light" = "shade-low", "Shade-high light" = "shade-high"),selected="shade-low")
     ),
     
     mainPanel(plotOutput('plot1'))
   )
))

server <-  shinyServer(function(input, output) {
    
  leaf_ss<- reactive({subset(cond_dat2, leaflight %in% input$whichleaf)
                      })
  
  output$plot1 <- renderPlot({
    
    palette(c(lightscol, shacol, suncol))
    
    par(mar=c(5,5,1,1),las=1,cex.axis=1.25, cex.lab=1.75)  
    plot(cond_dat2$Photo ~ cond_dat2$Cond,type='n',col="blue", ylim=c(5,25), xlim=c(0,.5), 
         xlab=expression(italic(g)[s]~~(mol~m^-2~s^-1)),
         ylab=expression(italic(A[n])~~(mu*mol~m^-2~s^-1)))
    
    legend("topleft", alllab2, pch=c(16,16,16,16,17), col=allcols2, inset = 0.01, 
           bty='n',pt.cex=1.5,cex=1.2)
    
    points(leaf_ss()[[2]]~ leaf_ss()[[3]], cex=2,  pch=c(16,17)[leaf_ss()[,4]],col=leaf_ss()[,1])
    
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

