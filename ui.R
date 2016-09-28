library(shiny)
library(shinyjs)

shinyUI(fluidPage(
    
    # init shinyjs
    useShinyjs(),
    
    # Application title
    titlePanel("Word prediction using stupid backoff algorithm"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            h4("Description"),
            p("This application, given an input text, gives a prediction of the next word that\
              should follow the text."),
            p(" "),
            h4("Usage"),
            p("Enter your text in the text box labeled ", em("Input phrase,"), "and press the ",
              em("Predict"), "button. A list of up to five predicted words, along with their \ 
                                  score (i.e., probability) is show."),
            p(strong("Notes:")),
            tags$ul(
                tags$li("The", em("Predict"), "button is disabled until initialization is \ 
                        complete - this might take a few seconds."),
                tags$li("Punctuation is ignored - it is neither taken into account for prediction,\
                        nor is shown as a predicted word.")
            )
            
        ),
        
        mainPanel(
            textInput("in.phrase", "Input phrase:"),
            disabled(actionButton("goButton", "Predict")),
            p(" "),
            p(" "),
            tags$br(),
            p(strong("Predicted words:")),
            tableOutput("pred")
        )
    )
))