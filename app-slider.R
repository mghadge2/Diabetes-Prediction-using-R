############################################
# Data Professor                           #
# http://youtube.com/dataprofessor         #
# http://github.com/dataprofessor          #
# http://facebook.com/dataprofessor        #
# https://www.instagram.com/data.professor #
############################################

# Import libraries
library(shiny)
library(data.table)
library(glmnet)
library(shinythemes)

# Read in the RF model
model <- readRDS("model.rds")
summary(model)

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]
head(TrainSet)
str(TrainSet)

####################################
# User interface                   #
####################################

ui2 <- fluidPage(theme = shinytheme("united"),
           navbarPage("Diabetes Predictor",
                      tabPanel("Random Forest",
                 pageWithSidebar(
  
  # Page header
  headerPanel(''),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input parameters</h4>"),
    sliderInput("BMI", label = "BMI", round=TRUE, value = 24.0,
                min = min(TrainSet$BMI),
                max = max(TrainSet$BMI)
    ),
    sliderInput("Age", label = "Age", round=TRUE, step=1, value = 3.0,
                min = 1,
                max = max(TrainSet$Age)
    ),
    selectInput("HighBP", label = "HighBP",
                choices = list("No" = "0", "Yes" = "1"), 
                selected = "No"),
    selectInput("HighChol", label = "HighChol",
                choices = list("No" = "0", "Yes" = "1"), 
                selected = "No"),
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
                 )
), #tabpanel RF

tabPanel("KNN"
) #tabpanel KNN
           ) # navbarPage()
) # fluidPage()

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("BMI",
               "Age",
               "HighBP",
               "HighChol"),
      Value = as.character(c(input$BMI,
                             input$Age,
                             input$HighBP,
                             input$HighChol)),
      stringsAsFactors = FALSE)
    
    Diabetes_binary <- 0
    df <- rbind(df, Diabetes_binary)
    print(df)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui2, server = server)