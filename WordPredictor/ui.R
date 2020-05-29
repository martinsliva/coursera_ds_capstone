# Coursera Capstone Data Science project
# Martin Slíva

library(shiny)
library(shinythemes)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    theme = shinytheme("darkly"),
    
    # Application title
    titlePanel("Word Prediction Application"),
    
    
    tabsetPanel(
        
        
        tabPanel("Application",
                 verticalLayout(
                     
                     textInput("input_text", "Write text here", "To be or not ", width = "800px"),
                     #verbatimTextOutput("value"),
                     
                     h4(textOutput("output_temp")),
                     br(),
                     
                     tableOutput("output_text")
                     
                     
                     
                     
                 )
            
        ),
        
        tabPanel("Mobil View",
              
                 flowLayout(
                     
                     textInput("input_text_m", "Write text here", "To be or not ", width = "800px"),
                     #verbatimTextOutput("value"),
                

                     tableOutput("output_text_m")
                     
                    
                     
                 )
         ),
                 
         tabPanel("Help",
                  
                  h4("Help for application"),
                  br(),
                  p("Input text to input box."),
                  p("Application guess next word."),
                  br(),
                  br(),
                  h4("Contact to author"),
                  br(),
                  a(href = "https://www.linkedin.com/in/martinsliva/", "Linkedin profile")
                  
         ),
        
        tabPanel("Documentation",
                 
                 h4("Overview"),
                 br(),
                 p("Application WordPredictor is trying to guess next word while you are typing into input box."),
                 p("Application was developed as a capstone project of Coursera Data Science Specialization led by Johns Hopkins University."),
                 br(),
                 br(),
                 h4("Slide deck"),
                 br(),
                 p("Slide deck with more information can be found at "), a( href = "https://rpubs.com/msliva/DataScienceCapstone", "RPubs slide deck."),
                 br(),
                 br(),
                 h4("Link to source code"),
                 br(),
                 p("Source code is stored in "), a( href = "https://github.com/martinsliva/coursera_ds_capstone", "Github repository.")
                 
                 
        )
        )
        
       
))
