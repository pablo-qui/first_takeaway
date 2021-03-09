#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(plotly)
library(ggplot2)
data(crabs)
c <- tabPanel("1",
              fluidPage(
                  sidebarLayout(
                      sidebarPanel(h2("hello")),
                      mainPanel(plotlyOutput("first")))
                  )#fluidpanel
            
              )#tab panel
d <- tabPanel("2")
refer <- tabPanel("References",
         p(tags$button(class="btn btn-default", 
                       `data-toggle`="collapse", 
                       `data-target`="#hola",
                       "References")),
         div(class="collapse", id="hola",
             div(class="card card-body",
                 includeMarkdown("references.md")
             )))#refer

# Define UI for application that draws a histogram
ui <- navbarPage("Shiny App on the Crabs Dataset",
    c,
    d,
    refer
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$first <- renderPlotly({
        ggplot(data = crabs,aes(x=CW,y=FL, color = sp))+geom_point()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
