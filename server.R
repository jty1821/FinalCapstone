library(shiny)
library(data.table)

# Define server logic required to make a prediction
shinyServer(function(input, output) {
  # Load the prediction function
  # source('nextWordsApp.R')
  nextWords <- function(rawStr, n, nGramAll) {
    
    ## [A] Remove numbers and punctuations
    filtList <- gsub('[[:punct:]]|[[:digit:]]', "", tolower(rawStr))
    # strsplit by all white spaces
    filtList <- unlist(strsplit(filtList, "\\s+"))
    
    ## [B] Extract last 6 words for query
    if (length(filtList) > 6) {
      filtList <- filtList[(length(filtList)-5):length(filtList)] #make query length 6
      filtStr <- paste(filtList, collapse = " ") #combine back to sentence
    } else {
      filtStr <- paste(filtList, collapse = " ") #combine back to sentence
    }
    
    ## [C] Returns all the matched words
    predText <- nGramAll[filtStr == nGramAll$query, ]$predict
    if (length(predText) > 0) {
      #hit with 7 gram
      finalText <- predText
    } else {
      #no hits
      filtStr <- paste(filtList[2:length(filtList)], collapse = " ") #remove 1st word
      predText <- nGramAll[filtStr == nGramAll$query, ]$predict
      if (length(predText) > 0) {
        #hit with 6 gram
        finalText <- predText
      } else {
        #no hits
        filtStr <- paste(filtList[3:length(filtList)], collapse = " ") #remove 2nd word
        predText <- nGramAll[filtStr == nGramAll$query, ]$predict
        if (length(predText) > 0) {
          #hit with 5 gram
          finalText <- predText
        } else {
          #no hits
          filtStr <- paste(filtList[4:length(filtList)], collapse = " ") #remove 3rd word
          predText <- nGramAll[filtStr == nGramAll$query, ]$predict
          if (length(predText) > 0) {
            #hit with 4 gram
            finalText <- predText
          } else {
            #no hits
            filtStr <- paste(filtList[5:length(filtList)], collapse = " ") #remove 4th word
            predText <- nGramAll[filtStr == nGramAll$query, ]$predict
            if (length(predText) > 0) {
              #hit with 3 gram
              finalText <- predText
            } else {
              #no hits
              filtStr <- paste(filtList[6:length(filtList)], collapse = " ") #remove 5th word (one word left)
              predText <- nGramAll[filtStr == nGramAll$query, ]$predict
              if (length(predText) > 0) {
                #hit with 2 gram
                finalText <- predText
              } else {
                #no hits
                finalText <- 'the' #most common word
              }
            }
          }
        }
      }  
    }
    return(finalText[1:n])
  }
  
  # Load the prediction table
   nGramAll <- fread('predictionTableFull.csv')
  # nGramAll <- fread('predictionTableUni.csv')
  
  # Predict next word
  observe({ #need this for reactive mode!!!
    query <- as.character(input$query)
    n <- input$wordN
    result <- nextWords(query, n, nGramAll)
    
    if (query == '') {
      output$predicted <- renderPrint(cat(''))
    } else {
      output$predicted <- renderPrint(cat(result, sep = '\n'))
    }
    
    })
})
