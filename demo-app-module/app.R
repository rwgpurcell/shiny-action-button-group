library(shiny)
source("../buttonSetModule.R")

ui <- fluidPage(
  h2("Click the buttons to add Points"),
  fluidRow(
    column(2,
           buttonSetModuleUI("butt")
      # actionButton("button2", "2"),br(),
      # actionButton("button3", "3"),br(),
      # actionButton("button4", "4"),br(),
      # actionButton("button5", "5")
    ),
    column(10,
      hr(),
      textOutput("buttonValue"),
      br(),
      plotOutput("plot")
    )
  )
)

server <- function(input, output){

  buttonData <- callModule(buttonSetModule,"butt",c("One"=1,"Two"=2,"Three"=3,"Four"=4,"Five"=5))
  
  #If you want an action triggered for repeat presses of the same button, the eventExpr needs to contain something that changes every press
  buttonSetAction <- observeEvent(c(buttonData$lastPress,buttonData$presses),{
    req(buttonData$lastPress)
    v$data <- c(v$data,rnorm(5,mean = buttonData$lastPress))
  }) 
  
  v <- reactiveValues(data = c())
  
  output$plot <- renderPlot({
    if (is.null(v$data)) return()
    hist(v$data)
  })
}

shinyApp(ui, server)
