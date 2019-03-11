# Load packages ----
library(shiny)
library(quantmod)

# Source helpers ----
source("helpers.R")

# User interface ----




library(shiny)
library(useful)


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyUI(fluidPage(
  
  # Application title
  titlePanel("The Amazing Spiral!"),
  
  # Sidebar with a slider input for number of bins
  
  sidebarLayout(
    sidebarPanel(
      helpText("Changing N over-writes the values of alpha and delta and n."),
      sliderInput("N",
                  "Number of edges:",
                  min = 1,
                  max = 30,
                  value = 3)
      ,
      sliderInput("alpha",
                  "length expansion factor:",
                  min = 0,
                  max = 0.05,
                  step = 0.001,
                  value = 0.01)
      ,
      sliderInput("delta",
                  "degree:",
                  min = 0,
                  max = 360,
                  step = 0.1,
                  value = 61)
      ,
      numericInput("n",
                   "num of itterations:",
                   value = 100,
                   min = 0, max = 10000, step =1)
      ,
      numericInput("teta",
                   "initital degree:",
                   value = 0,
                   min = -180, max = 180, step = 1)
    ),
    
    
    mainPanel(
      div(style = "height: 900px;",
          plotOutput("distPlot", height = "100%")
          
      )
    )
  )
))


shinyServer(function(input, output,session) {
  
  ## Updates the delta and alpha based on N
  N <- reactive({
    N <- input$N
    val <- 360/N +1/N
    updateSliderInput(session, "delta", value = val)
    
    val <-  0.03/N
    updateSliderInput(session, "alpha", value = val)
    updateNumericInput(session, "n", value = N*200)
    N
  })
  
  ## genarates the points in polar coordinations, then convert to cartesian, can update the 
  # alpha and delta if changed by user
  cart <-  reactive({
    N <- N()
    alpha <- input$alpha
    delta <- input$delta
    teta <- input$teta
    n <- input$n
    t <-  -delta + teta
    r <- 1
    p <- data.frame(r  = numeric(n), tet = numeric(n), col = numeric(n))
    for (i in 1:n){
      r <-  r + alpha*r
      t <- delta + t
      p$r[i] <-  r
      p$tet[i] <- t
      p$col[i] <- (i%% round(360/delta))
    }
    cart <- pol2cart(r = p$r,theta = p$tet,degrees = T)
    cart$col <- p$col
    cart
  })
  
  
  output$distPlot <- renderPlot({
    cart <- cart()
    cart
    ggplot(cart, aes(x,y, color = factor(col)), asp = 1)+ geom_path()+ aes(group=NA) + 
      theme(aspect.ratio=1,panel.background = element_blank())
  })
  
  
 
})


# Run the app
shinyApp(ui, server)
