fieldsMandatory <- c("gender", "age", "white_blood_cell", "monocyte", "lymphocyte", "c_reactive_protein", "creatine")
proteins <- head(filbin_numeric, 1)


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
        
        appendTab(inputId = "tabs",
                  tabPanel("Results for Patient",
                           titlePanel("Your results"),
                           #p(sprintf("We estimate you have a %s chance of being %s.", rate, health))
                           )
                  )
        
        
        updateNavbarPage(session, "navbar",
                         selected="results")
    })
    

    observeEvent(input$gbutton, {
        if (input$gbutton == 0) {
            return()
        }
        
        appendTab(inputId = "tabs",
                  tabPanel("Results for General Practitioner",
                           titlePanel("Your results"),
                           p("If non-healthy we've assessed you as: "),
                           verbatimTextOutput("severity"),
                           p("To review your inputted proteome: "),
                           fluidRow(
                               column(4, tableOutput("input_df"))
                           ),
                           p("Accuracy of model used for diagnosis: "),
                           plotOutput("accuracy")
                           )
        )
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
        output$input_df <- renderTable({
            head(filbin_numeric, 1)
        })
        
        output$severity = renderPrint({
            model <- reactive({
                predict(trained_svm, head(filbin_numeric, 1)[, selected_features]) %>%
                    as.character()
            }); model()
        })
        
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