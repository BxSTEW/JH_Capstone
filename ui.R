suppressPackageStartupMessages(c(
    library(shinythemes),
    library(shiny)
))

# give the application a title and browser tab text
appTitle = (div(HTML("<center>Coursera Johns Hopkins Capstone Project</center>")))
browserText = "Capstone"

# create tabs and panels
shinyUI(fluidPage(titlePanel(appTitle,browserText),
                  
                  hr(), # styling
                  tags$head(tags$style(HTML("
    #final_text {
      text-align: center;
    }
    div.box-header {
      text-align: center;
    }
    "))),
                  
                  theme = shinytheme("slate"),
                  
                  navbarPage("Next Word Prediction",id ="navpanel",
                             
                             # Home tab is panel with a sidebar and main sections  
                             tabPanel(
                                      sidebarLayout(
                                          
                                          #sidebar - Instructions 
                                          sidebarPanel(id="sidebarPanel"
                                                       , includeHTML("./Instructions.html")
                                          ), 
                                          
                                          # mainpanel - text prediction app
                                          mainPanel(id="mainpanel",
                                                    tags$div(textInput("text", 
                                                                       label = h4("Input"),
                                                                       value = ),
                                                             br(),
                                                             tags$hr(),
                                                             
                                                             h4("Suggested next word:"),
                                                             tags$span(style="color:red",
                                                                       tags$strong(tags$h3(textOutput("nextWords")))),
                                                             br(),
                                                             tags$hr(),
                                                             
                                                             h4("Your input:"),
                                                             tags$span(style="color:red",
                                                                       tags$em(tags$h3(textOutput("inputWords")))),
                                                             align="center"))
                                      ),

                             ),
                            
                             
                             tags$hr()
                  )))
