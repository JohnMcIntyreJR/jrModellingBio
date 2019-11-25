library(ggplot2)
# library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(shiny)

shinyUI(fluidPage(
  titlePanel("Regression"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head( tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                 tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')
      ),
      p("A simple linear regression example using the mtcars cars data set\n.
      Points in red are those which are deemed to have large influence on the model fit, that is cooks distance $D > 4/n$\n. 
      You can drag an area around points and use the toggle below to change whether they are included or not."),
      
      actionButton("exclude_toggle", "Toggle points"),
      actionButton("exclude_reset", "Reset")
    )
    ,
    
    mainPanel(
      plotOutput("plot1", height = 350,
                 click = "plot1_click",
                 brush = brushOpts(
                   id = "plot1_brush"
                 )
      )
    )
  )
))
