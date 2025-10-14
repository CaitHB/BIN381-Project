#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(caret)
#nutrition model
saved_nutrition <- readRDS("finalModels/rf_nutrition_model.rds")  
rf_nutrition <- saved_nutrition$model  # Extract the model
feature_names <- saved_nutrition$feature_names  # Extract feature names
levels_data <- saved_nutrition$levels  # Extract levels

xgb_sanitation <- readRDS("finalModels/xgb_sanitation_model.rds")
logit_health <- readRDS("finalModels/logit_healthA_model.rds")
rf_social <- readRDS("finalModels/rf_model_GroupD_Social.rds")

print(rf_nutrition)
# Define UI for application
ui <- fluidPage(
  titlePanel("Four Models Prediction App"),
  
  # Four blocks
  fluidRow(
    column(3,
           actionButton("show_nutrition", "Nutrition Model", width = "100%"),
           style = "background-color: #E6F2FF; padding: 20px; text-align:center;"),
    column(3,
           actionButton("show_sanitation", "Sanitation Model", width = "100%"),
           style = "background-color: #FFE6E6; padding: 20px; text-align:center;"),
    column(3,
           actionButton("show_health", "Health Model", width = "100%"),
           style = "background-color: #E6FFE6; padding: 20px; text-align:center;"),
    column(3,
           actionButton("show_social", "Social Model", width = "100%"),
           style = "background-color: #FFF5E6; padding: 20px; text-align:center;")
  ),
  
  hr(),
  
  # Dynamic forms
  uiOutput("nutrition_ui"),
  uiOutput("sanitation_ui"),
  uiOutput("health_ui"),
  uiOutput("social_ui")
)

  
  server <- function(input, output, session) {
    # Create reactive values to store indcat_levels
    values <- reactiveValues(indcat_levels = levels_data$IndicatorCategoryHigh)
    
    observeEvent(input$show_nutrition, {
      output$nutrition_ui <- renderUI({
        tagList(
          h3("Nutrition Model Inputs"),
          helpText("Fill in the following features to predict Nutrition Value:"),
          numericInput("nut_den_weight", "Denominator Weighted (DW):", value = 1000, min = 0),
          numericInput("nut_den_unweight", "Denominator Unweighted (DU):", value = 1000, min = 0),
          if (!is.null(values$indcat_levels) && length(values$indcat_levels) > 0) {
            selectInput("nut_indcat", "Indicator Category High:", choices = values$indcat_levels, selected = values$indcat_levels[1])
          } else {
            helpText("Error: Indicator categories not available.")
          },
          selectInput("nut_indicator_type", "Indicator Type:", choices = levels_data$IndicatorType),
          selectInput("nut_characteristic_category", "Characteristic Category:", choices = levels_data$CharacteristicCategory),
          selectInput("nut_population_group", "Population Group:", choices = levels_data$PopulationGroup),
          numericInput("nut_survey_year", "Survey Year:", value = 2020, min = 2000, max = 2030),
          actionButton("predict_nutrition", "Predict Nutrition")
        )
      })
    })
    
    observeEvent(input$predict_nutrition, {
      req(input$nut_den_weight, input$nut_den_unweight, input$nut_indcat, input$nut_indicator_type, input$nut_characteristic_category, input$nut_population_group, input$nut_survey_year)
      
      newdata <- data.frame(
        IndicatorType = factor(input$nut_indicator_type, levels = levels_data$IndicatorType),  # Explicit factor
        DenominatorWeighted = as.numeric(input$nut_den_weight),
        DenominatorUnweighted = as.numeric(input$nut_den_unweight),
        CharacteristicCategory = factor(input$nut_characteristic_category, levels = levels_data$CharacteristicCategory),
        PopulationGroup = factor(input$nut_population_group, levels = levels_data$PopulationGroup),
        SurveyYear = as.numeric(input$nut_survey_year),  # Assuming numeric; change if factor
        IndicatorCategoryHigh = factor(input$nut_indcat, levels = values$indcat_levels)
      )
      
      print("Newdata structure for prediction:")  # Debugging
      print(str(newdata))  # Check types and levels in console
      print("Expected features from training:")  # For comparison
      print(saved_nutrition$feature_names)  # Ensure they match
      
      tryCatch({
        pred <- predict(rf_nutrition, newdata)
        showModal(modalDialog(
          title = "Nutrition Prediction",
          paste("Predicted Nutrition Value:", round(pred, 2)),
          easyClose = TRUE
        ))
      }, error = function(e) {
        showNotification(paste("Prediction error:", e$message), type = "error")
      })
    })
    
    
    # Sanitation Model
    observeEvent(input$show_sanitation, {
      output$sanitation_ui <- renderUI({
        tagList(
          h3("Sanitation Model Inputs"),
          numericInput("san_dw", "Denominator Weighted:", value = 1000, min = 0),
          numericInput("san_du", "Denominator Unweighted:", value = 1000, min = 0),
          actionButton("predict_sanitation", "Predict Sanitation")
        )
      })
      output$nutrition_ui <- renderUI(NULL)
      output$health_ui <- renderUI(NULL)
      output$social_ui <- renderUI(NULL)
      
      showNotification("Sanitation model loaded.", type = "message")
    })
    
    observeEvent(input$predict_sanitation, {
      req(input$san_dw, input$san_du)
      
      newdata <- data.frame(
        DenominatorWeighted   = input$san_dw,
        DenominatorUnweighted = input$san_du
      )
      
      if(any(is.na(newdata))) {
        showNotification("Please fill all Sanitation inputs!", type = "error")
      } else {
        tryCatch({
          dmat <- xgb.DMatrix(data = as.matrix(newdata))
          pred <- predict(xgb_sanitation, dmat)
          showModal(modalDialog(
            title = "Sanitation Prediction",
            paste("Predicted Value:", round(pred, 2)),
            easyClose = TRUE
          ))
        }, error = function(e) {
          showNotification(paste("Prediction error:", e$message), type = "error")
        })
      }
    })
    
    # Health Model
    observeEvent(input$show_health, {
      output$health_ui <- renderUI({
        tagList(
          h3("Health Model Inputs"),
          numericInput("health_val", "Value:", value = 50, min = 0),
          numericInput("health_year", "Survey Year:", value = 2015, min = 1900, max = 2100),
          actionButton("predict_health", "Predict Health")
        )
      })
      output$nutrition_ui <- renderUI(NULL)
      output$sanitation_ui <- renderUI(NULL)
      output$social_ui <- renderUI(NULL)
      
      showNotification("Health model loaded.", type = "message")
    })
    
    observeEvent(input$predict_health, {
      req(input$health_val, input$health_year)
      
      newdata <- data.frame(
        Value = input$health_val,
        SurveyYear = input$health_year
      )
      
      if(any(is.na(newdata))) {
        showNotification("Please fill all Health inputs!", type = "error")
      } else {
        tryCatch({
          pred_prob <- predict(logit_health, newdata, type = "response")
          pred_class <- ifelse(pred_prob >= 0.5, "High", "Low")
          showModal(modalDialog(
            title = "Health Prediction",
            paste("Predicted Class:", pred_class, "(Probability:", round(pred_prob, 2), ")"),
            easyClose = TRUE
          ))
        }, error = function(e) {
          showNotification(paste("Prediction error:", e$message), type = "error")
        })
      }
    })
    
    # Social Model (Updated as per your request)
    observeEvent(input$show_social, {
      output$social_ui <- renderUI({
        tagList(
          h3("Social Model Inputs"),
          fluidRow(  # Incorporate the provided fluidRow for descriptive labels
            column(6,
                   selectInput(
                     "social_charcat", 
                     "Characteristic Category (1 = Male, 2 = Female):",
                     choices = c("1", "2")
                   ),
                   helpText("Select the gender for this record. 1 = Male, 2 = Female.")
            ),
            column(6,
                   numericInput(
                     "social_value", 
                     "Survey Year:",
                     value = 2020, min = 2000, max = 2030
                   ),
                   helpText("Enter the year of the survey for prediction.")
            )
          ),
          actionButton("predict_social", "Predict Social")
        )
      })
      output$nutrition_ui <- renderUI(NULL)
      output$sanitation_ui <- renderUI(NULL)
      output$health_ui <- renderUI(NULL)
      
      showNotification("Social model loaded.", type = "message")
    })
    
    observeEvent(input$predict_social, {
      req(input$social_charcat, input$social_value)  # Ensure new inputs are provided
      
      newdata <- data.frame(
        CharacteristicCategory = factor(input$social_charcat, levels = c("1", "2")),  # Treat as factor for consistency
        SurveyYear = input$social_value
      )
      
      if(any(is.na(newdata))) {
        showNotification("Please fill all Social inputs!", type = "error")
      } else {
        tryCatch({
          pred <- predict(rf_social, newdata)
          showModal(modalDialog(
            title = "Social Prediction",
            paste("Predicted Value:", round(pred, 2)),
            easyClose = TRUE
          ))
        }, error = function(e) {
          showNotification(paste("Prediction error:", e$message), type = "error")
        })
      }
    })
  }
  
  shinyApp(ui, server)