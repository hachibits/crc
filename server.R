fieldsMandatory <- c("gender", "age", "white_blood_cell", "monocyte", "lymphocyte", "c_reactive_protein", "creatine")
proteins <- head(filbin_numeric, 1)
rate = round(mean(pred.svm.prob <- attr(pred.svm, 'probabilities')[,1])*100, 4)

shinyServer(function(input, output, session) {
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
        #shinyjs::toggleState(id="gbutton", condition=mandatoryFilled)
    })
    

    observeEvent(input$pbutton, {
        if (input$pbutton == 0) {
            return()
        }
        
        output$health <- renderPrint({
            prediction <- reactive({
                pred.svm <- predict(model.svm, head(pdata, 1), probability=TRUE)
                as.character(pred.svm)
            }); prediction()
        })
        
        appendTab(inputId = "tabs",
                  tabPanel("Results for Patient",
                           titlePanel("Your results"),
                           p(sprintf("We estimate a %s %% chance of you being:", rate)),
                           textOutput("health"),
                           br(),
                           br(),
                           h4("If healthy, here's how you can prevent contraction of COVID-19:"),
                           HTML("<ul>
                                    <li>
                                        Clean your hands often. Use soap and water, or an alcohol-based hand rub.
                                    </li>
                                    <li>
                                        Maintain a safe distance from anyone who is coughing or sneezing.
                                    </li>
                                    <li>
                                        Wear a mask when physical distancing is not possible.
                                    </li>
                                    <li>
                                        Don’t touch your eyes, nose or mouth.
                                    </li>
                                    <li>
                                        Cover your nose and mouth with your bent elbow or a tissue when you cough or sneeze.
                                    </li>
                                    <li>
                                        Stay home if you feel unwell.
                                    </li>
                                    <li>
                                        If you have a fever, cough and difficulty breathing, seek medical attention.
                                    </li>
                                    <li>
                                        Wear masks.
                                    </li>
                                </ul>")
                           )
                  )
        
        
        updateNavbarPage(session, "navbar",
                         selected="results")
    })
    

    observeEvent(input$gbutton, {
        if (input$gbutton == 0) {
            return()
        }
        
        # Input file parsing ----
        # output$input_df <- renderTable({
        #     req(input$target_upload)
        #     
        #     df <- read.csv(input$target_upload$datapath,
        #                    skip = 1,
        #                    sep = input$separator)
        #     
        #     reduced_df <- df %>%
        #         as.data.frame()
        #         dplyr::select(which(colnames(df) %in% colnames(filbin_numeric)))
        # 
        #     return(reduced_df)
        # })
        output$input_df <- DT::renderDataTable({
            DT::datatable(head(filbin_numeric, 1), 
                          options = list(dom = 't'))
        })
        
        
        input_df <- as.data.frame(input$target_upload) %>%
            subset(select = -1)
        
        ncov <- reactive({
            predict(model.svm, input_df, probability=TRUE) %>%
                as.character()
        })
        
        severity <- reactive({
            input_df <- input_df %>%
                dplyr::select(which(colnames(input_df) %in% selected_features))
            predict(trained_svm, input_df) %>%
                as.character()
        })
        
        if (ncov() == "COVID-19") {
            output$severity = renderPrint({
                severity();
            })
            
            # UI pagination ----
            appendTab(inputId = "tabs",
                tabPanel("Results for General Practitioner",
                 titlePanel("Your assessment"),
                 br(),
                 
                 p(sprintf("We've assessed your case to have a %s %% chance of being:", round(mean(svm_acc), 4)*100)),
                 htmlOutput("severity"),
                 
                 linebreaks(2),
                 
                 h4("Review your inputted proteome: "),
                 fluidRow(
                     column(width = 12,
                            shinydashboard::box(
                                width = NULL, status = "primary",
                                div(style = 'overflow-x: scroll', DT::dataTableOutput("input_df"))
                            )
                     )
                 ),
                 
                 tags$hr(), 
                 
                 h4("Accuracy of model used for diagnosis: "),
                 plotOutput("accuracy")
                )
            )
            
        } else {
            appendTab(inputId = "tabs",
                tabPanel("Results for General Practitioner",
                    titlePanel("Your assessment"),
                    br(),
                    
                    p(sprintf("We've assessed you to be COVID-free with %s accuracy, however it's in your best interests to get an official check up and keep up with regulations.", rate)),
                    
                    linebreaks(2),
                    
                    h4("Review your inputted proteome: "),
                    fluidRow(
                        column(width = 12,
                               shinydashboard::box(
                                   width = NULL, status = "primary",
                                   div(style = 'overflow-x: scroll', DT::dataTableOutput("input_df"))
                               )
                        )
                    ),
                    
                    tags$hr(), 
                    
                    h4("Accuracy of model used for diagnosis: "),
                    plotOutput("accuracy")
                      
                )
            )
        }

        output$accuracy <- renderPlot({
            plot = boxplot(svm_acc,
                    horizontal = TRUE, xlab = "Accuracy",
                    names = c("SVM"))
        })
        
        
        updateNavbarPage(session, "navbar",
                         selected="results")
        
    })
    
    # Button to download (row of on-hand data/feature labels only)
    output$download <- downloadHandler(
        filename = "scaffold.csv",
        content = function(filename) {
            write.csv(proteins, filename)
        }
    )
    
})
