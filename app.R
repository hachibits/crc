library(shiny)
library(tidyverse)
load("dat.Rdata")

fieldsMandatory <- c("gender", "age")

labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class="mandatory_star")
    )
}

appCSS <-
    ".mandatory_star { color: red; }"

shinyApp(
    ui <- navbarPage(
        title = "COVID-19 Risk Calculator",
        id = "navbar",
        tabPanel(
            title = "Patient Form",
            fluidPage(
                shinyjs::useShinyjs(),
                shinyjs::inlineCSS(appCSS),
                titlePanel(""),
                div(
                    id="form",
                    
                    selectInput("gender", "Gender", c("", "Male", "Female")),
                    textInput("age", "Age", "")
                )
            ),
            actionButton("button", "Submit")
        ),
        tabPanel(
            title = "Results",
            value = "result",
            verbatimTextOutput("results")
        )
    ),
    
    server <- function(input, output, session) {
        observe({
            mandatoryFilled <- 
                vapply(fieldsMandatory,
                       function(x) {
                           !is.null(input[[x]]) && input[[x]] != ""
                       },
                       logical(1))
            mandatoryFilled <- all(mandatoryFilled)
            
            shinyjs::toggleState(id="submit", condition=mandatoryFilled)
        })
        
        observeEvent(input$button, {
            updateNavbarPage(session, "navbar",
                             selected="result")
        })
        
        output$results <- renderPrint({
            p(input$gender, input$age)
        })
    }
)

shinyApp(ui = ui, server = server)