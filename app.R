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
    ui <- fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        titlePanel("COVID-19 Risk Calculator"),
        div(
            id="form",
            
            selectInput("gender", "Gender", c("", "Male", "Female")),
            textInput("age", "Age", ""), 
            actionButton("submit", "Submit", class="btn-primary")
        )
    ),
    
    server <- function(input, output) {
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
        # TODO: classification logic
    }
)

shinyApp(ui = ui, server = server)
