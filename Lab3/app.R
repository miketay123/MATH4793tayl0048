#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output)

    formulaText <- reactive({
    paste("theta", input$variable)
        })
library(mvtnorm)
    mat=rmvnorm(100,mean=c(10,10))
    sn=cov(mat)
    x1=mat[,1]
    x2=mat[,2]
    s12tilde=sn[1,1]*cos(theta)*sin(theta)-sn[1,2]*cos(theta)^2+sn[1,2]*sin(theta)^2-sn[2,2]*cos(theta)*sin(theta)
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- theta
        bins <- seq(min(x), max(x), length.out = input$bins + 1)


    })


# Run the application
shinyApp(ui = ui, server = server)
