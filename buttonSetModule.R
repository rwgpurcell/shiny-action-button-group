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
}