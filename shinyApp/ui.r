library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Minnesota Football By Decade"),
  
  sidebarPanel(
    
    sliderInput(inputId = "yr.sub", 
                label = "Years",
                min = 1882, 
                max = 2012,
                value = c(2000, 2009)      
      ),
    
    wellPanel(
      selectInput(inputId = "y.var",
                  label = "Y Variable",
                  choices = c("Points Scored" = "PF",
                              "Points Against" = "PA"),
                  selected = "Points Scored")
      ),
    
    checkboxInput(inputId = "W.L",
                  label = "Identify Win/Loss",
                  value = FALSE),
    
    submitButton("Update Graph")
    
    ),
  
  mainPanel(
    plotOutput(outputId = "main_plot")
    )
  
  ))