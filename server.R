suppressPackageStartupMessages(c(
    library(stringr),
    library(stylo),
    library(tm),
    library(shiny)
))

source("./Predict.R")

shinyServer(function(input, output) {
    nextWord <- reactive({
        t <- input$text
        tt <- reFreshen(t)
        wCount <- length(tt)
        nextWord <- getNextWord(wCount,tt)})
    
    output$nextWords <- renderPrint(nextWord())
    output$inputWords <- renderText({input$text}, quoted = FALSE)
    
})