library(shiny)

ui <- fluidPage(
  fluidRow(
    column(2,
      actionButton("button1", "1"),br(),
      actionButton("button2", "2"),br(),
      actionButton("button3", "3"),br(),
      actionButton("button4", "4"),br(),
      actionButton("button5", "5")
    ),
    column(10,
      hr(),
      textOutput("buttonValue"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output){

  buttonData <- reactiveValues(presses=0)
  
  output$buttonValue <- renderText(buttonData$lastPress)
  
  for(i in 1:5){
    local({
      my_i <- i
      observeEvent(input[[paste0("button",my_i)]], {
        buttonData$presses <- buttonData$presses +1
        buttonData$lastPress <- my_i
      })
    })
  }
  
  # observeEvent(input$button1, {
  #   buttonData$presses <- buttonData$presses +1
  #   buttonData$lastPress <- 1
  # })
  # 
  # observeEvent(input$button2, {
  #   buttonData$presses <- buttonData$presses +1
  #   buttonData$lastPress <- 2
  # })
  # 
  # observeEvent(input$button3, {
  #   buttonData$presses <- buttonData$presses +1
  #   buttonData$lastPress <- 3
  # })
  # 
  # observeEvent(input$button4, {
  #   buttonData$presses <- buttonData$presses +1
  #   buttonData$lastPress <- 4
  # })
  # 
  # observeEvent(input$button5, {
  #   buttonData$presses <- buttonData$presses +1
  #   buttonData$lastPress <- 5
  # })
  # 
  
  buttonSetAction <- observeEvent(c(buttonData$lastPress,buttonData$presses),{
    v$data <- rnorm(10)
  }) 
  
  v <- reactiveValues(data = NULL)
  
  # observeEvent(input$button2, {
  #   v$data <- rnorm(100)
  # })  
  
  output$plot <- renderPlot({
    if (is.null(v$data)) return()
    hist(v$data)
  })
}

shinyApp(ui, server)
