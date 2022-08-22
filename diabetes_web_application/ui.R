library(shiny)
library(data.table)
library(caret)
library(e1071)
library(caTools)

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

data <- read.csv("diabetes.csv")

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(data$Outcome, p=0.8, list = FALSE)
TrainingSet <- data[TrainingIndex,] # Training Set


pageWithSidebar(
  
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