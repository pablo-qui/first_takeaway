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
library(shinythemes)

data(crabs)
colnames(crabs)

intro <- tabPanel("Description",
                  fluidPage(theme = shinytheme("flatly"),
                    mainPanel(
                    p("The crabs data frame has 200 rows and 8 columns, describing 5 morphological measurements on
50 crabs each of two colour forms and both sexes, of the species Leptograpsus variegatus collected
at Fremantle, W. Australia."),
br(),
p("This data frame contains the following columns:"),
br(),
p("- sp species: B or O for blue or orange."),
p("- sex: M for male, F for female."),
p("- index: index 1:50 within each of the four groups."),
p("- FL: frontal lobe size (mm)."),
p("- RW: rear width (mm)."),
p("- CL: carapace length (mm)."),
p("- CW: carapace width (mm)."),
p("- BD: body depth (mm).")
                    )#aminpanel
                  )#fluidpage
                  )#tabpanel

table <- tabPanel("Table",
        fluidPage(
        fluidRow(
            column(6, selectInput("sex","Sex",c("All",levels(crabs$sex)))),
            column(6,selectInput("sp","Species",c("All",levels(crabs$sp))))
        ),#fluidRow
        dataTableOutput("table")
        )#fluidpage
        )#tabpanel


plot <- tabPanel("Plots",
                 sidebarLayout(
                    sidebarPanel(h4("Plot the different variables against each other 
                                    differenciating between Sex or Species"),
                      selectInput("varx",
                        "Variable on X",
                        c("Frontal Lobe Size", "Rear Width","Carapace Length",
                        "Carapace Width", "Body Depth")), #select input 
                      selectInput("vary",
                                  "Variable on Y",
                                  c("Frontal Lobe Size", "Rear Width","Carapace Length",
                                    "Carapace Width", "Body Depth"),
                                  selected = "Rear Width"),
                      radioButtons("rb", "Differenciate between Sex or Species",
                                   c("Sex","Species"))#radiobuttons
                        ),#side bar panel
                    mainPanel(plotlyOutput("plot")))
                 #fluidpanel
        )#tab panel

reg <- tabPanel("Regression Model",
                )#tabpanel

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
    intro,
    table,
    plot,
    reg,
    refer
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    selectx <- reactive({
        switch (input$varx,
            "Frontal Lobe Size" = crabs$FL,
            "Rear Width" = crabs$RW,
            "Carapace Length" = crabs$CL,
            "Carapace Width" = crabs$CW,
            "Body Depth" = crabs$BD
        )
    })
    selecty <- reactive({
        switch (input$vary,
            "Frontal Lobe Size" = crabs$FL,
            "Rear Width" = crabs$RW,
            "Carapace Length" = crabs$CL,
            "Carapace Width" = crabs$CW,
            "Body Depth" = crabs$BD
            )
    })
    col <- reactive({
        switch (input$rb,
            "Sex" = crabs$sex,
            "Species" = crabs$sp
        )
    })
    
    output$plot <- renderPlotly({
        ggplot(data = crabs,aes(x=selectx(),y=selecty(), color=col()))+geom_point()+
            xlab(input$varx)+ylab(input$vary)+labs(color =input$rb)
    })
    
    output$table <- renderDataTable({
        crabs
        if (input$sex!="All"){
           crabs <-  crabs[crabs$sex==input$sex,]
        }
        if (input$sp!="All"){
           crabs <-  crabs[crabs$sp==input$sp,]
        }
        crabs
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
