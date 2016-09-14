source("appWordPredictor.r")

########################################
########################################

# Load all models and encoder
gram4Model <- loadModel(4)
gram3Model <- loadModel(3)
gram2Model <- loadModel(2)
gram1Model <- loadModel(1)
encoder <- readRDS("dictionaries/encoder_40.Rds")

########################################
########################################

nextWords <- function(userText, profFilter = FALSE) 
{
    return(simpleLinearInterpolation(userText, 
                                     gram4Model, 
                                     gram3Model, 
                                     gram2Model, 
                                     gram1Model, 
                                     encoder,
                                     profFilter))
}

########################################
########################################

shinyServer(function(input, output, session) 
{
    suggestions <- eventReactive(input$goBtn,
    {
        if (input$profanity)    {return(nextWords(input$userText, TRUE))}
        else                    {return(nextWords(input$userText, FALSE))}
    })
    
    clearAll <- eventReactive(input$clearBtn,
    {
        return("")
    })

    suggNumb <- eventReactive(input$suggNumber,
    {
        return(input$suggNumber)
    })
    
    addWord1 <- eventReactive(input$resultBtn1,
    {
        return(paste(input$userText, names(suggestions())[1]))
    })
    
    addWord2 <- eventReactive(input$resultBtn2,
    {
        return(paste(input$userText, names(suggestions())[2]))
    })
    
    addWord3 <- eventReactive(input$resultBtn3,
    {
        return(paste(input$userText, names(suggestions())[3]))
    })
    
    addWord4 <- eventReactive(input$resultBtn4,
    {
        return(paste(input$userText, names(suggestions())[4]))
    })
    
    addWord5 <- eventReactive(input$resultBtn5,
    {
        return(paste(input$userText, names(suggestions())[5]))
    })
    
    #----------#
    
    observe(
    {
        updateTextInput(session, "userText", value = addWord1())
    })
    observe(
    {
        updateTextInput(session, "userText", value = addWord2())
    })
    observe(
    {
        updateTextInput(session, "userText", value = addWord3())
    })
    observe(
    {
        updateTextInput(session, "userText", value = addWord4())
    })
    observe(
    {
        updateTextInput(session, "userText", value = addWord5())
    })
    observe(
    {
        updateTextInput(session, "userText", value = clearAll())
    })
    
    #----------#
    
    output$resultBtn1 <- renderUI(
    {
        word <- names(suggestions())[1]
        actionButton("resultBtn1", word, class="btn-success")
    })
    output$resultBtn2 <- renderUI(
    {
        if (input$suggNumber > 1)   
        {
            word <- names(suggestions())[2]
            actionButton("resultBtn2", word)
        }
    })
    output$resultBtn3 <- renderUI(
    {
        if (input$suggNumber > 1)   
        {
            word <- names(suggestions())[3]
            actionButton("resultBtn3", word)
        }
    })
    output$resultBtn4 <- renderUI(
    {
        if (input$suggNumber > 3)   
        {
            word <- names(suggestions())[4]
            actionButton("resultBtn4", word)
        }
    })
    output$resultBtn5 <- renderUI(
    {
        if (input$suggNumber > 3)   
        {
            word <- names(suggestions())[5]
            actionButton("resultBtn5", word)
        }
    })

    
    #----------#
    
    output$details1 <- renderText({paste(names(suggestions())[1], "-", suggestions()[1])})
    output$details2 <- renderText(
    {
        if (input$suggNumber > 1)   {paste(names(suggestions())[2], "-", suggestions()[2])}
    })
    output$details3 <- renderText(
    {
        if (input$suggNumber > 1)   {paste(names(suggestions())[3], "-", suggestions()[3])}
    })
    output$details4 <- renderText(
    {
        if (input$suggNumber > 3)   {paste(names(suggestions())[4], "-", suggestions()[4])}
    })
    output$details5 <- renderText(
    {
        if (input$suggNumber > 3)   {paste(names(suggestions())[5], "-", suggestions()[5])}
    })
    
    #----------#
    
    output$detailsTable1 <- renderDataTable(
    {
        if (input$suggNumber == 1) {detailsDT <- data.frame(words = names(suggestions()[1]), scores = round(suggestions()[1], 5))}
    }, options = list(searching = FALSE, ordering = FALSE, processing = FALSE, paging = FALSE, info = FALSE))
    
    output$detailsTable3 <- renderDataTable(
    {
        if (input$suggNumber == 3) {detailsDT <- data.frame(words = names(suggestions())[1:3], scores = round(suggestions()[1:3], 5))}
    }, options = list(searching = FALSE, ordering = FALSE, processing = FALSE, paging = FALSE, info = FALSE))
    
    output$detailsTable5 <- renderDataTable(
    {
        if (input$suggNumber == 5) {detailsDT <- data.frame(words = names(suggestions())[1:5], scores = round(suggestions()[1:5], 5))}
    }, options = list(searching = FALSE, ordering = FALSE, processing = FALSE, paging = FALSE, info = FALSE))
})
