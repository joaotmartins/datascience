library(shiny)


shinyUI(fluidPage(
    
    # Application title
    titlePanel("Word prediction using stupid backoff algorithm"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            h4("Prediction")
        ),
        
        mainPanel(
            h4("Input"),
            textInput("in.phrase", "Input phrase"),
            actionButton("goButton", "Predict"),
            p(),
            tableOutput("pred")
        )
    )
))