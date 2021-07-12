library(shiny)
library(markdown)

shinyUI(navbarPage("Natural Language Processing & Prediction: Data Science Capstone",
                   tabPanel("Text prediction app",
                            img(src = 'headers.png'),
                            sidebarLayout(
                              sidebarPanel(
                                textInput("EnterText","Enter a word or part of a phrase below", value = ""),
                                submitButton("Predict Next Word")
                                
                                
                              ),
                              
                              mainPanel(
                                h2("Suggested next word:"),
                                verbatimTextOutput("prediction"),
                                textOutput('text1')
                              )
                            )
                            
                            
                            
                            
                            ),
                   tabPanel("Overview",
                            img(src = 'headers.png'),
                            mainPanel(
                              includeMarkdown("Overview.md")
                            ))
                  
                   
                   
                   ))
