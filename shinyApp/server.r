library(ggplot2)
library(proto)
library(scales)
library(RColorBrewer)

load("MinnFootball.Rdata")

# Returns a logical vector of which values in `x` are within the min and max
# values of `range`.
in_range <- function(x, range) {
  x >= min(range) & x <= max(range)
}

#minnFootball$Decade <- with(minnFootball, ifelse(Year %in% 1880:1889, 1, ifelse(
#  Year %in% 1890:1899, 2, ifelse(Year %in% 1900:1909, 3, ifelse(
#    Year %in% 1910:1919, 4, ifelse(Year %in% 1920:1929, 5, ifelse(
#      Year %in% 1930:1939, 6, ifelse(Year %in% 1940:1949, 7, ifelse(
#        Year %in% 1950:1959, 8, ifelse(Year %in% 1960:1969, 9, ifelse(
#          Year %in% 1970:1979, 10, ifelse(Year %in% 1980:1989, 11, ifelse(
#            Year %in% 1990:1999, 12, ifelse(Year %in% 2000:2009, 13, 14))))))))))))))

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
      
    ## label Win/loss
    if(input$W.L){
      aes.map.point <- geom_jitter(mapping = aes_string(color = "W.L", shape = "W.L"))
      manual.color <- scale_color_brewer(palette = "Set1")
    } else {
      aes.map.point <- geom_jitter()
      manual.color <- NULL
    }
    
    
    ## draw base plot
    p <- ggplot(minnFootball.sub, aes(x = factor(Year), y = PF)) + 
      geom_boxplot() + theme_bw() + aes.map.point + 
      xlab("Year") + manual.color
    print(p)
  })
  
  
  

})