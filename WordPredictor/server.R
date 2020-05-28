#
# This is the server logic of a Shiny web application. 
# 
#

library(shiny)


# Define server 


shinyServer(function(input, output) {

    output$output_text <- renderTable({
        
        temp <- str_replace_all(input$input_text, regex("[^-a-zA-Z']"), " ") # replaces all but a-z, A-Z, and ' -
        
        temp <- str_replace_all(temp, "  +", " ") ## replaces more spaces by one

        a <- unlist(str_split(tolower(temp), " "))
        a <- c(rep("<unk>", 5) , a ) ## adding some <unk> tokens to be able predict word for short input 
        b <- length(a)
        if (a[b] == "") {  ## clean the input string
                b <- b - 1
                a <- a[1:(b)]
        }
            
             cap_val_output(find_word(a[b - 3], a[b - 2], a[b - 1], a[b], no_word = no_words)$word1) 

    }, colnames = FALSE)
    
    output$output_temp <- renderText(input$input_text)

    output$output_text_m <- renderTable({
        
        temp <- str_replace_all(input$input_text_m, regex("[^-a-zA-Z']"), " ") # replaces all but a-z, A-Z, and ' -
        
        temp <- str_replace_all(temp, "  +", " ") ## replaces more spaces by one
        
        a <- unlist(str_split(tolower(temp), " "))
        a <- c(rep("<unk>", 5) , a ) ## adding some <unk> tokens to be able predict word for short input 
        b <- length(a)
        if (a[b] == "") {  ## clean the input string
            b <- b - 1
            a <- a[1:(b)]
        }
        
        cap_val_output(find_word(a[b - 3], a[b - 2], a[b - 1], a[b], no_word = no_words)$word1) 
        
    }, colnames = FALSE)
    
    output$output_temp <- renderText("Word Suggestion:")
    
    
    
})
