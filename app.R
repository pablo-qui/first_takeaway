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
library(tidyverse)
library(shinyjs)
library(class)
library(caret)

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
                 sidebarLayout(position = 'right',
                    sidebarPanel(h4("Plot the different variables against each other 
                                    differenciating between Sex or Species."),
                                 h5("You can also compare the means for the different variables
                                    between groups in the table below the graph."),
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
                    mainPanel(plotlyOutput("plot"),
                              fluidRow(column(12,offset=2,
                              tableOutput("table2"))))
                 )#sidebar layout
        )#tab panel

reg <- tabPanel("Regression Model",
                sidebarLayout(position = 'right',
                    sidebarPanel(p("Here, the objective is to run k-nearest neighbours to determine 
                                   the species of the crab based on the other variables."),
                        useShinyjs(),
                        sliderInput("knn","Number of Neighbours",
                                    min = 1, max=7,step=1, value=1),
                        checkboxInput("split", label = "Automatical Split Size", value = TRUE),
                        sliderInput("split2","Select Splitting  ",
                            min=0.5, max = 0.9, step=0.01, value=0.75),
                        downloadButton("report", "Generate report")),
                    mainPanel(h3("Confusion Matrix of the Regression"),verbatimTextOutput("confm"))
                )#side bar layout
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
    
    output$table <- renderDataTable({
        crabs
        if (input$sex!="All"){
            crabs <-  crabs%>%filter(sex==input$sex)
        }
        if (input$sp!="All"){
            crabs <-  crabs%>%filter(sp==input$sp)
        }
        crabs
    })
    
    
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
    
    
    output$table2 <- renderTable({
        crabs %>% group_by(col()) %>% summarize(mean_FL = mean(FL), mean_RW=mean(RW),
                                mean_CL=mean(CL), mean_CW=mean(CW), mean_BD=mean(BD))
    })
    
    
    
    
    observe(toggle("split2", input$split, anim = TRUE, animType = "slide"))
    
    output$confm <- renderPrint({
        if (!input$split){
            r <- input$split2
        } else { r <- 0.75}
        
        crabs$sp <- as.numeric(crabs$sp) - 1
        crabs$sex <- as.numeric(crabs$sex) - 1
        
        spl = createDataPartition(crabs$sp, p = r, list = FALSE)
        
        XTrain = crabs[spl,-3]
        XTest = crabs[-spl,-3]
        knn_pred <- knn(train = scale(XTrain[,-1]), test = scale(XTest[,-1]), cl = XTrain$sp, k=input$knn)
        levels(knn_pred) <- c("B","0")
        XTest <- as.factor(XTest$sp)
        levels(XTest) <- c("B","0")
        confusionMatrix(knn_pred, XTest)
    })
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(
                size_spl = if (!isolate(input$split)){isolate(input$split2)} else {0.75},
                k = isolate(input$knn)
            )
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
