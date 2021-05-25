library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
load("dat.Rdata")

plabel <- as.data.frame(filbin_data$group)
ID <- plabel != "non-COVID-19"
filbin_data <- filbin_data[ID, ]
# Intersect structure of dataframes ----
shen_numeric <- shen_data %>%
    dplyr::select(which(colnames(shen_data) %in% colnames(filbin_data))) %>%
    dplyr::select(where(is.numeric))
filbin_numeric <- filbin_data %>%
    dplyr::select(which(colnames(shen_data) %in% colnames(filbin_data))) %>%
    dplyr::select(where(is.numeric))
# Log-transformation ----
filbin_numeric = log2(filbin_numeric[rowSums(is.na(filbin_numeric)) < 306*0.5,])
# Median normalisation ----
pmedian <- apply(filbin_numeric, 2, median, na.rm = TRUE)
adj <- pmedian - median(pmedian)
filbin_numeric <- sweep(filbin_numeric, 2, adj, FUN = "-")

## Model building & training ----
# Extract train and test data
covid_id <- !(filbin_data$group == "non-COVID-19")
X <- as.matrix(filbin_numeric[covid_id, ])

rownames(X) <- paste("sample", seq_len(nrow(X)))
y <- filbin_data$group[covid_id]
names(y) <- paste("sample", seq_len(nrow(X)))

test_id <- cvSets$subsets[cvSets$which == j]
X_test <- X[test_id, ]
X_train <- X[-test_id, ]
y_test <- y[test_id]
y_train <- y[-test_id]

design <- model.matrix(~ y_train)

# Fit the limma model ----
fit <- lmFit(t(X_train), design)
fit2 <- eBayes(fit)
tT <- topTable(fit2, coef = 2, number = Inf, sort.by ="t")
selected_features <- rownames(tT)[1:200]

# Support vector machine ----
pred_svm <- c()
trained_svm <- svm(X_train[, selected_features],
                   factor(y_train), type = "C")
predicted_svm <- predict(trained_svm, X_test[, selected_features])
names(predicted_svm) <- names(y_test)
pred_svm <- append(pred_svm, predicted_svm)

fieldsMandatory <- c("gender", "age", "white_blood_cell", "monocyte", "lymphocyte", "c_reactive_protein", "creatine", "target_upload")

shinyApp(
    ui <- navbarPage(
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
               p("This proteomics risk calculator provides a reflection of patient health, current COVID-19 status "),
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
                          radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=";",inline=TRUE),
#                          DT::dataTableOutput("output_table"),

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
            value = "result",
            #verbatimTextOutput("results")
            fluidRow(
                column(4, tableOutput('results'))
            )
        )
    ),
    
    server <- function(input, output, session) {
        # Mandatory user input checking and validation ---- 
        observe({
            mandatoryFilled <- 
                vapply(fieldsMandatory,
                       function(x) {
                           !is.null(input[[x]]) && input[[x]] != ""
                       },
                       logical(1))
            mandatoryFilled <- all(mandatoryFilled)
            
            shinyjs::toggleState(id="pbutton", condition=mandatoryFilled)
            shinyjs::toggleState(id="gbutton", condition=mandatoryFilled)
        })
        
        observeEvent(input$pbutton | input$gbutton, {
            if (input$pbutton == 0 && input$gbutton == 0) {
                return()
            }
            updateNavbarPage(session, "navbar",
                             selected="result")
        })
        
        # # Input file parsing ----
        # file <- input$target_upload
        # ext <- tools::file_ext(file$datapath)
        # 
        # req(file)
        # validate(need(ext == "csv", "Please upload a .csv file"))
        # 
        # df <- read.csv(file$datapath)
        
        output$results <- renderTable({
           pred_svm
        })
    }
)

shinyApp(ui = ui, server = server)