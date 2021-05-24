library(shiny)
library(shinythemes)
library(tidyverse)
load("dat.Rdata")

fieldsMandatory <- c("gender", "age", "white_blood_cell", "monocyte", "lymphocyte", "c_reactive_protein", "creatine")

shinyApp(
    ui <- navbarPage(
        title = "COVID-19 Risk Calculator",
        id = "navbar",
        tabPanel(
            title = "Patient Form",
            fluidPage(
                shinyjs::useShinyjs(),
                theme = shinytheme("flatly"),
                titlePanel("Fill in below for a COVID-19 risk assessment."),
                h4("Intended for the general public."),
                br(),
                div(
                    id="patient-form",
                    
                    fluidRow(
                        column(8, align="center", offset = 2,
                               
                               selectInput("gender", "Gender", c("", "Male", "Female")),
                               selectInput("age", "Age", c("", "20 - 34", "35 - 49", "50 - 64", "65 - 79", "80+")),
                               selectInput("white_blood_cell", "White blood cell count", c("", "0 - 0.99", "1 - 3.99", "4-7.99", "8 - 11.99", "12+")),
                               selectInput("monocyte", "Monocytes count", c("", "0 - 0.24", "0.25 - 0.49", "0.5 - 0.74", "0.75 - 0.99", "1+")),
                               selectInput("lymphocyte", "Lymphocyte count", c("", "0 - 0.49", "0.5 - 0.99", "1 - 1.49", "1.5 -1.99", "2+")),
                               selectInput("c_reactive_protein", "C-reactive protein (CRP)", c("", "0 - 19", "20 - 59", "60 - 99", "100 - 179", "180+")),
                               selectInput("creatine", "Creatine", c("", "0 - 0.79", "0.8-1.19", "1.2-1.79", "1.8-2.99", "3+")),
                               
                               tags$style(type="text/css", "
                                          #string { 
                                            height: 50px; 
                                            width: 100%; 
                                            text-align: center; 
                                            font-size: 30px;    
                                            display: block;
                                          }")
                        )
                    )
                    
                )
            ),
            br(),
            
            fluidRow(
                column(6, align="center", offset = 3,
                       actionButton("pbutton", "Calculate"),
                       tags$style(type='text/css', "
                                  #button { 
                                    vertical-align: middle; 
                                    height: 50px; 
                                    width: 100%; 
                                    font-size: 30px;
                                  }")
                )
            )
        ),
        
        tabPanel(
           title = "General Practitioner Form",
           fluidPage(
               titlePanel("Fill in below for a COVID-19 risk assessment."),
               h4("Intended for the medical expert in charge of patient."),
               br(),
               div(
                   id="gp-form",
                   
                   fluidRow(
                       column(8, align="center", offset = 2,
                              
                              # TODO: take protein input thru user given csv 
                              
                              tags$style(type="text/css", "
                                         #string { 
                                           height: 50px; 
                                           width: 100%; 
                                           text-align: center; 
                                           font-size: 30px;
                                           display: block;
                                         }")
                       )
                   )
                   
               )
           ),
           br(),
           
           fluidRow(
               column(6, align="center", offset = 3,
                      actionButton("gbutton", "Calculate"),
                      tags$style(type='text/css', "
                                 #button { 
                                   vertical-align: middle; 
                                   height: 50px; 
                                   width: 100%; 
                                   font-size: 30px;
                                 }")
               )
           )
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
            
            shinyjs::toggleState(id="pbutton", condition=mandatoryFilled)
        })
        
        observeEvent(input$pbutton | input$gbutton, {
            if (input$pbutton == 0 && input$gbutton == 0) {
                return()
            }
            updateNavbarPage(session, "navbar",
                             selected="result")
        })
        
        output$results <- renderPrint({
            p(input$gender, input$age)
        })
    }
)

shinyApp(ui = ui, server = server)