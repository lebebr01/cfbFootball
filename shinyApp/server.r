library(ggplot2)

load("MinnFootball.Rdata")

shinyServer(function(input, output) {
  
  geom_boxplot <- function (mapping = NULL, data = NULL, stat = "boxplot", position = "dodge",
                            outlier.colour = NULL, outlier.shape = NULL, outlier.size = NULL,
                            notch = FALSE, notchwidth = .5, ...) {
    outlier_defaults <- Geom$find('point')$default_aes()
    outlier.colour <- outlier.colour %||% outlier_defaults$colour
    outlier.shape <- outlier.shape %||% outlier_defaults$shape
    outlier.size <- outlier.size %||% outlier_defaults$size
    GeomBoxplot$new(mapping = mapping, data = data, stat = stat, 
                    position = position, outlier.colour = outlier.colour, outlier.shape = outlier.shape, 
                    outlier.size = outlier.size, notch = notch, notchwidth = notchwidth, ...)
  }
    
    GeomBoxplot <- proto(Geom, {
      objname <- "boxplot"
      
      reparameterise <- function(., df, params) {
        df$width <- df$width %||% 
          params$width %||% (resolution(df$x, FALSE) * 0.9)
        
        if (!is.null(df$outliers)) {
          suppressWarnings({
            out_min <- vapply(df$outliers, min, numeric(1))
            out_max <- vapply(df$outliers, max, numeric(1))
          })
          
          df$ymin_final <- pmin(out_min, df$ymin)
          df$ymax_final <- pmax(out_max, df$ymax)
        } 
        
        transform(df,
                  xmin = x - width / 2, xmax = x + width / 2, width = NULL
        )
        
      }
      
      draw <- function(., data, ..., fatten = 2, outlier.colour = NULL, outlier.shape = NULL, outlier.size = 2,
                       notch = FALSE, notchwidth = .5) { 
        common <- data.frame(
          colour = data$colour, 
          size = data$size, 
          linetype = data$linetype,
          fill = alpha(data$fill, data$alpha),  
          group = data$group,
          stringsAsFactors = FALSE
        )
        
        whiskers <- data.frame(
          x = data$x,
          xend = data$x, 
          y = c(data$upper, data$lower), 
          yend = c(data$ymax, data$ymin),
          alpha = NA,
          common)
        
        box <- data.frame(
          xmin = data$xmin, 
          xmax = data$xmax, 
          ymin = data$lower, 
          y = data$middle, 
          ymax = data$upper,
          ynotchlower = ifelse(notch, data$notchlower, NA),
          ynotchupper = ifelse(notch, data$notchupper, NA),
          notchwidth = notchwidth,
          alpha = data$alpha, 
          common)
        
        if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
          outliers <- data.frame(
            y = data$outliers[[1]],
            x = data$x[1],
            colour = outlier.colour %||% data$colour[1],
            shape = outlier.shape %||% data$shape[1],
            size = outlier.size %||% data$size[1],
            fill = NA,
            alpha = NA,
            stringsAsFactors = FALSE)
          outliers_grob <- GeomPoint$draw(outliers, ...)
        } else {
          outliers_grob <- NULL
        }
        
        ggname(.$my_name(), grobTree(
          outliers_grob,
          GeomSegment$draw(whiskers, ...),
          GeomCrossbar$draw(box, fatten = fatten, ...)
        ))
      }
      
      guide_geom <- function(.) "boxplot"  
      draw_legend <- function(., data, ...)  {
        data <- aesdefaults(data, .$default_aes(), list(...))
        gp <- with(data, gpar(col=colour, fill=alpha(fill, alpha), lwd=size * .pt, lty = linetype))
        gTree(gp = gp, children = gList(
          linesGrob(0.5, c(0.1, 0.25)),
          linesGrob(0.5, c(0.75, 0.9)),
          rectGrob(height=0.5, width=0.75),
          linesGrob(c(0.125, 0.875), 0.5)
        ))
      }
      
      default_stat <- function(.) StatBoxplot
      default_pos <- function(.) PositionDodge
      default_aes <- function(.) aes(weight=1, colour="grey20", fill="white", size=0.5, alpha = NA, shape = 16, linetype = "solid")
      required_aes <- c("x", "lower", "upper", "middle", "ymin", "ymax")
      
    })
  
  output$main_plot <- renderPlot({
    
    ## Limit Range of data to a specific decade
    minnFootball.sub <- subset(minnFootball, Year %in% input$yr.sub)
    
    ## label Win/loss
    if(input$W.L){
      aes.map.point <- aes_string(color = "W.L", shape = "W.L")
      manual.color <- scale_color_brewer(palette = "Set1")
    } else {
      aes.map.point <- aes_string()
      manual.color <- ""
    }
    
    
    ## draw base plot
    p <- ggplot(minnFootball.sub, aes(x = factor(Year), y = PF)) + 
      geom_boxplot() + theme_bw() + geom_jitter(mapping = aes.map.point) + 
      xlab("Year") + manual.color
    print(p)
  })
  
  
  

})