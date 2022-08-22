library(shiny)
library(data.table)
library(caret)
library(e1071)
library(caTools)

# Read in the RF model
model <- readRDS("model.rds")

shinyServer(function(input, output, session) {
  
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
  
})