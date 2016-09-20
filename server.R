library(shiny)

shinyServer(function(input, output) {
    
    prediction <- reactive({
        if(input$goButton >= 1) {
            isolate({
                if (nchar(input$in.phrase) != 0) {
                    input$in.phrase
                } else {
                    "(nothing)"
                }
            })
        }
    })
    
    output$pred <- prediction
})