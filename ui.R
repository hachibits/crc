library(shiny)
library(shinythemes)

shinyUI(
    navbarPage(
        title = "COVID-19 Risk Calculator",
        id = "navbar",
        
        # Patient Form UI ---- 
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
        
        # GP Form UI ----
        tabPanel(
            title = "General Practitioner Form",
            fluidPage(
                titlePanel("Fill in below for a COVID-19 risk assessment."),
                h3("COVID-19 Risk Calculator"),
                p("This COVID-19 risk calculator is catered to your local GP - provides a reflection of patient health, current COVID-19 status and vulnerability of contracting the disease. To conduct this measure, we request a series of proteomics data for the desired outcome, a boilerplate spreadsheet is available below to be assessed and filled by your GP."),
                # Download scaffold csv ----
                br(),
                fluidRow(
                    column(8, align="center", offset = 2,
                           downloadButton('download', "Download scaffold .csv")
                    )
                ),
                h3("Disclaimer"),
                p("Intended for the medical expert in charge of patient. Interpretation of the results of this calculator by those without appropriate medical and/or clinical training is not recommended."),
                br(),
                div(
                    id="gp-form",
                    
                    fluidRow(
                        column(8, align="center", offset = 2,
                               
                               fileInput('target_upload', 'Choose file to upload',
                                         accept = c(
                                             'text/csv',
                                             'text/comma-separated-values',
                                             '.csv'
                                         )),
                               # Select delimiter ----
                               radioButtons("separator","Separator: ",
                                            choices = c(";",",",":"), 
                                            selected=",",
                                            inline=TRUE),
                               
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
        
        # Final results ouput ----
        tabPanel(
            title = "Results",
            value = "results",
            # verbatimTextOutput("severity")
            fluidRow(
                column(4, tableOutput("input_df"))
            )
        )
    )
)