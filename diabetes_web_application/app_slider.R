# Import libraries
library(shiny)
library(data.table)
library(caret)
library(e1071)
library(caTools)

# Read in the RF model
model <- readRDS("model.rds")

# Training set
TrainSet <- read.csv("diabetes.csv")
TrainSet <- TrainSet[,-1]

data <- read.csv("diabetes.csv")

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(data$Outcome, p=0.8, list = FALSE)
TrainingSet <- data[TrainingIndex,] # Training Set


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Diabetes Predictor'),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input parameters</h4>"),
    sliderInput("Pregnancies", label = "Pregnancies", value = 0,
                min = min(TrainingSet$Pregnancies),
                max = max(TrainingSet$Pregnancies)
    ),
    sliderInput("Glucose", label = "Glucose", value = 0,
                min = min(TrainSet$Glucose),
                max = max(TrainSet$Glucose)
    ),
    sliderInput("BloodPressure", label = "BloodPressure", value = 0,
                min = min(TrainSet$BloodPressure),
                max = max(TrainSet$BloodPressure)
    ),
    sliderInput("SkinThickness", label = "SkinThicknes", value = 0,
                min = min(TrainSet$SkinThicknes),
                max = max(TrainSet$SkinThicknes)
    ),
    sliderInput("Insulin", label = "Insulin", value = 0,
                min = min(TrainSet$Insulin),
                max = max(TrainSet$Insulin)
    ),
    sliderInput("BMI", label = "BMI", value = 0,
                min = min(TrainSet$BMI),
                max = max(TrainSet$BMI)
    ),
    sliderInput("DiabetesPedigreeFunction", label = "DiabetesPedigreeFunction", value = 0.078,
                min = min(TrainSet$DiabetesPedigreeFunction),
                max = max(TrainSet$DiabetesPedigreeFunction)
    ),
    sliderInput("Age", label = "Age", value = 5.0,
                min = min(TrainSet$Age),
                max = max(TrainSet$Age)
    ),
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata'), # Prediction results table
    
tabPanel("About", 
      titlePanel("About"), 
      div(includeMarkdown("about.md"), 
      align="justify")) #tabPanel(), About
    
  )
  
  
  
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Pregnancies",
               "Glucose",
               "BloodPressure",
               "SkinThickness",
               "Insulin",
               "BMI",
               "DiabetesPedigreeFunction",
               "Age"),
      Value = as.character(c(input$Pregnancies,
                             input$Glucose,
                             input$BloodPressure,
                             input$SkinThickness,
                             input$Insulin,
                             input$BMI,
                             input$DiabetesPedigreeFunction,
                             input$Age)),
      stringsAsFactors = FALSE)
    
    Outcome <- 0
    df <- rbind(df, Outcome)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test))
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
shinyApp(ui = ui, server = server)