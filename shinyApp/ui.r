library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Minnesota Football By Decade"),
  
  sidebarPanel(
    
    wellPanel(
      selectInput(inputId = "yr.sub",
                  label = "Decades",
                  choices = c("1880's" = 1880:1889,
                              "1890's" = 1890:1899,
                              "1900's" = 1900:1909,
                              "1910's" = 1910:1919,
                              "1920's" = 1920:1929,
                              "1930's" = 1930:1939,
                              "1940's" = 1940:1949,
                              "1950's" = 1950:1959,
                              "1960's" = 1960:1969,
                              "1970's" = 1970:1979,
                              "1980's" = 1980:1989,
                              "1990's" = 1990:1999,
                              "2000's" = 2000:2009,
                              "2010's" = 2010:2019),
                   selected = "2010's")
      ),
    
    wellPanel(
      selectInput(inputId = "y.var",
                  label = "Y Variable",
                  choices = c("Points Scored" = "PF",
                              "Points Against" = "PA"),
                  selected = "Points Scored")
      )
    
    checkboxInput(inputId = "W.L",
                  label = "Identify Win/Loss",
                  value = FALSE)
    ),
  
  mainPanel(
    plotOutput(outputId = "main_plot"),
    )
  
  ))