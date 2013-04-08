library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Minnesota Football By Decade"),
  
  sidebarPanel(
    
    selectInput(inputId = "x.var",
                label = "X Variable", 
                choices = c("Year" = "Year",
                            "Opponent" = "Opponent"),
                selected = "Year"),
    
    selectInput(inputId = "y.var",
                  label = "Y Variable",
                  choices = c("Points Scored" = "PF",
                              "Points Against" = "PA",
                              "Difference in Points" = "Delta"),
                  selected = "Points Scored"),
    
    sliderInput(inputId = "yr.sub", 
                label = "Years",
                min = 1882, 
                max = 2012,
                value = c(2000, 2009)      
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