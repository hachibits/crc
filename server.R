fieldsMandatory <- c("gender", "age", "white_blood_cell", "monocyte", "lymphocyte", "c_reactive_protein", "creatine")
proteins <- head(filbin_numeric, 1)
df <- read.csv("./scaffold.csv")
predicted_svm <- predict(trained_svm, df[, selected_features]) %>%
    as.character()

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
    
    # observeEvent(input$pbutton | input$gbutton, {
    #     if (input$pbutton == 0 && input$gbutton == 0) {
    #         return()
    #     }
    #     updateNavbarPage(session, "navbar",
    #                      selected="results")
    # })
    observeEvent(input$gbutton, {
        if (input$gbutton == 0) {
            return()
        }
        
        # Input file parsing ----
        # model <- reactive({ 
        #     # file = input$target_upload
        #     # if (is.null(file)) 
        #     #     return(NULL)
        #     # 
        #     # ext = tools::file_ext(file$datapath)
        #     # req(file)
        #     # validate(need(ext == "csv", "Please upload a .csv file"))
        #     
        #     df = reactive({
        #         read.csv(file$datapath, skip=1, header=TRUE, sep=input$separator)
        #     })
        #     
        #     input_df = df()
        # 
        #     # predicted_svm <- predict(trained_svm, input_df[, selected_features])
        #     
        #     return(predicted_svm)
        # })
        output$input_df <- renderTable({
            req(input$target_upload)
            
            df <- read.csv(input$target_upload$datapath,
                           sep = input$separator)
            
            return(df)
        })
        
        # output$severity = renderTable({
        #     severity <- model(); severity 
        # })
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