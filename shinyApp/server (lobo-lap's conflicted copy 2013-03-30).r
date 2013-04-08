library(ggplot2)
library(scales)
library(RColorBrewer)
library(data.table)

load("MinnFootball.Rdata")
minnFootball$Opponent <- as.character(minnFootball$Opponent)
minnFootball$Year <- as.character(minnFootball$Year)
minnFootball$Date <- as.Date(minnFootball$Date, format = "%m-%d-%Y")
minnFootball <- data.table(minnFootball, key = c("Date", "Opponent"))
minnFootball <- unique(minnFootball)

minnFootball$Delta <- with(minnFootball, PF-PA)

cols <- brewer.pal(3, "Set1")
cols2 <- c("L" = cols[1], "T" = cols[2], "W" = cols[3])
shapes <- c("L" = 16, "T" = 17, "W" = 15)

shinyServer(function(input, output) {
  
  subsetData <- function(){
    ## Years to subset on
    years.limits <- as.numeric(unlist(strsplit(paste(input$yr.sub, 
                                                     collapse = ' '), ' ')))
    years.sub <- seq(years.limits[1], years.limits[2], by = 1)
    
    ## Limit Range of data to a specific decade
    minnFootball.sub <- subset(minnFootball, Year %in% years.sub)
    minnFootball.sub
    
  }
  
    output$main_plot <- renderPlot({
      
      minnFootball.sub <- subsetData()    
      
      aes.map <- aes_string(x = input$x.var, y = input$y.var)
      
      ## label Win/loss
      if(input$W.L){
        aes.map.point <- geom_jitter(mapping = aes_string(color = "W.L", shape = "W.L"))
        manual.color <- scale_color_manual(values = cols2) #+ 
          #scale_shape_manual(values = c(16, 17, 15), labels = c("L", "T", "W"))
      } else {
        aes.map.point <- geom_jitter()
        manual.color <- NULL
      }
      
      if(as.character(input$x.var) == "Opponent"){
        rotate.axis <- theme(axis.text.x = element_text(angle = 90, hjust = 1))
      } else {
        rotate.axis <- NULL
      }
      
      
      ## draw base plot
      p <- ggplot(minnFootball.sub, mapping = aes.map) + 
        geom_boxplot() + theme_bw() + aes.map.point + 
        xlab("Year") + manual.color + rotate.axis
      print(p)
      

  })
  

})