#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mediation)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Mediation"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("xInMed",
                        "X in Mediator",
                        min = 0,
                        max = 5,
                        value = 1,
                        step=0.1),
            sliderInput("xInY",
                        "X in Y",
                        min = 0,
                        max = 5,
                        value = 1,
                        step=0.1),
            sliderInput("medInY",
                        "Mediator in Y",
                        min = 0,
                        max = 5,
                        value = 1,
                        step=0.1),
            sliderInput("noise",
                        "Noise coefficient:",
                        min = 0,
                        max = 5,
                        value = 1,
                        step=0.1)
        ),
        mainPanel(
            plotOutput("rhoPlot"),
            verbatimTextOutput("medSumm"),
            verbatimTextOutput("sensSumm")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    makeMed <- reactive({
        x<-input$noise*rnorm(100,0,1)
        med<-(input$xInMed*x)+(input$noise*rnorm(100,0,1))
        y<-(input$xInY*x)+(input$medInY*med)+(input$noise*rnorm(100,0,1))
        df<-data.frame("x"=x,"y"=y,"med"=med)
        lm1<-lm(med~x,data=df)
        lm2<-lm(y~med+x,data=df)
        medObj <- mediate(lm1,lm2,sims=100,boot=TRUE,treat="x",mediator="med",outcome="y")
        return(medObj)
    })
    output$medSumm <- renderPrint({
        summary(makeMed())
    })
    output$sensSumm <- renderPrint({
        summary(medsens(makeMed()))
    })
    output$rhoPlot <- renderPlot({
        plot(medsens(makeMed()))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
