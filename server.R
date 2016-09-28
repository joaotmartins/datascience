library(shiny)
library(shinyjs)
source("lut_predict.R")

luts <- import.luts("data")
message("LUTs loaded.")

shinyServer(function(input, output) {
    
    enable("goButton")
    
    prediction <- reactive({
        if(input$goButton >= 1) {
            isolate({
                if (nchar(input$in.phrase) != 0) {
                    guess_word(input$in.phrase, luts)
                } else {
                    data.frame(word = "", score = 0)
                }
            })
        } else {
            data.frame(word = "", score = 0)
        }
        
    })
    
    output$pred <- renderTable(prediction(), digits = 4, width = 300)
})