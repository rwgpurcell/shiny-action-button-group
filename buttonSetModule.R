buttonSetModuleUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("buttonSet"))
}

buttonSetModule <- function(input, output, session, choices, inline=FALSE,n_buttons=NULL) {
  #set ns for use in renderUI outputs
  ns <- session$ns
  
  buttonData <- reactiveValues(presses=0,lastPress = NULL)
  
  nButtons <- length(choices)
  for(i in 1:nButtons){
    local({
      my_i <- i
      #nButtons: paste0("button",my_i)
      observeEvent(input[[names(choices)[my_i]]], {
        buttonData$presses <- buttonData$presses + 1
        buttonData$lastPress <- choices[my_i]
      })
    })
  }
  
  #nButtons: paste0("button",i)
  #inline: no br()s
  button_list <- lapply(1:nButtons, function(i) {
    tagList(actionButton(ns(names(choices)[i]), i),br())
  })
  
  output$buttonSet <- renderUI({
    button_list
  })
  
  return(buttonData)
  
  # # collect one year of data
  # ydata <- reactive({
  #   filter(data, year == input$year)
  # })
  # 
  # xrange <- range(data$gdpPercap)
  # yrange <- range(data$lifeExp)
  # 
  # output$plot <- renderPlot({
  #   
  #   # draw background plot with legend
  #   plot(data$gdpPercap, data$lifeExp, type = "n", 
  #        xlab = "GDP per capita", ylab = "Life Expectancy", 
  #        panel.first = {
  #          grid()
  #          text(mean(xrange), mean(yrange), input$year, 
  #               col = "grey90", cex = 5)
  #        })
  #   
  #   legend("bottomright", legend = levels(data$continent), 
  #          cex = 1.3, inset = 0.01, text.width = diff(xrange)/5,
  #          fill = c("#E41A1C99", "#377EB899", "#4DAF4A99", 
  #                   "#984EA399", "#FF7F0099"))
  #   
  #   # Determine bubble colors
  #   cols <- c("Africa" = "#E41A1C99",
  #             "Americas" = "#377EB899",
  #             "Asia" = "#4DAF4A99",
  #             "Europe" = "#984EA399",
  #             "Oceania" = "#FF7F0099")[ydata()$continent]
  #   
  #   # add bubbles
  #   symbols(ydata()$gdpPercap, ydata()$lifeExp, circles = sqrt(ydata()$pop),
  #           bg = cols, inches = 0.5, fg = "white", add = TRUE)
  # })
}