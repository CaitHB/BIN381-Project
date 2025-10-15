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

#sanitation(b)
feature_names <- readRDS("finalModels/xgb_sanitation_feature_names.rds") 
xgb_sanitation <- readRDS("finalModels/xgb_sanitation_model.rds")

#health (a)
logit_health <- readRDS("finalModels/logit_healthA_model.rds")

#social model (d)
rf_social <- readRDS("finalModels/rf_model_GroupD_Social.rds")


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
    values <- reactiveValues(indcat_levels = levels_data$IndicatorCategoryHigh)
    
    # NUTRITION MODEL
    observeEvent(input$show_nutrition, {
      output$nutrition_ui <- renderUI({
        tagList(
          h3("Nutrition Model Inputs"),
          helpText("Fill in the following features to predict Nutrition Value:"),
          
          numericInput("nut_den_weight", "Denominator Weighted (DW):", value = 1000, min = 0),
          numericInput("nut_den_unweight", "Denominator Unweighted (DU):", value = 1000, min = 0),
          
          # Conditional dropdown error handling
          if (!is.null(values$indcat_levels) && length(values$indcat_levels) > 0) {
            selectInput("nut_indcat", "Indicator Category High:", choices = values$indcat_levels)
          } else {
            helpText(" Error: Indicator categories not available. Please check model data.")
          },
          
          selectInput("nut_indicator_type", "Indicator Type:", choices = levels_data$IndicatorType),
          selectInput("nut_characteristic_category", "Characteristic Category:", choices = levels_data$CharacteristicCategory),
          selectInput("nut_population_group", "Population Group:", choices = levels_data$PopulationGroup),
          numericInput("nut_survey_year", "Survey Year:", value = 2020, min = 2000, max = 2030),
          actionButton("predict_nutrition", "Predict Nutrition")
        )
      })
      
      # Hide other panels
      output$sanitation_ui <- renderUI(NULL)
      output$health_ui <- renderUI(NULL)
      output$social_ui <- renderUI(NULL)
      
      showNotification("Nutrition model loaded.", type = "message")
    })
    
    observeEvent(input$predict_nutrition, {
      req(input$nut_den_weight, input$nut_den_unweight, input$nut_indicator_type,
          input$nut_characteristic_category, input$nut_population_group, input$nut_survey_year)
      
      # Create new data
      newdata <- data.frame(
        IndicatorType = factor(input$nut_indicator_type, levels = levels_data$IndicatorType),
        DenominatorWeighted = as.numeric(input$nut_den_weight),
        DenominatorUnweighted = as.numeric(input$nut_den_unweight),
        CharacteristicCategory = factor(input$nut_characteristic_category, levels = levels_data$CharacteristicCategory),
        PopulationGroup = factor(input$nut_population_group, levels = levels_data$PopulationGroup),
        SurveyYear = as.numeric(input$nut_survey_year),
        IndicatorCategoryHigh = factor(input$nut_indcat, levels = levels_data$IndicatorCategoryHigh)
      )
      
      # Try to predict and catch errors
      tryCatch({
        pred <- predict(rf_nutrition, newdata)
        showModal(modalDialog(
          title = "Nutrition Prediction",
          paste("Predicted Nutrition Value:", round(pred, 2)),
          easyClose = TRUE
        ))
      }, error = function(e) {
        showNotification(paste("Prediction failed:", e$message), type = "error", duration = 8)
      })
    })
    
    # SANITATION MODEL
    IndicatorType_levels <- c("I", "S", "T", "D", "U")  
    CharacteristicCategory_levels <- c("Total")  
    
    # Template for encoding
    temp_list <- list()
    if (length(IndicatorType_levels) > 1) {
      temp_list$IndicatorType <- factor("", levels = IndicatorType_levels)
    }
    if (length(CharacteristicCategory_levels) > 1) {
      temp_list$CharacteristicCategory <- factor("", levels = CharacteristicCategory_levels)
    }
    temp_list$DenominatorWeighted <- numeric(1)
    temp_list$DenominatorUnweighted <- numeric(1)
    temp_list$Weight_Ratio <- numeric(1)
    template_df <- as.data.frame(temp_list, stringsAsFactors = FALSE)
    dummies <- dummyVars(~ ., data = template_df)
    
    observeEvent(input$show_sanitation, {
      output$sanitation_ui <- renderUI({
        tagList(
          h3("Sanitation Model Inputs"),
          if ("IndicatorType" %in% names(template_df)) {
            selectInput("IndicatorType", "Indicator Type", choices = IndicatorType_levels)
          },
          if ("CharacteristicCategory" %in% names(template_df)) {
            selectInput("CharacteristicCategory", "Characteristic Category", choices = CharacteristicCategory_levels)
          },
          numericInput("san_dw", "Denominator Weighted:", value = 1000, min = 0),
          numericInput("san_du", "Denominator Unweighted:", value = 1000, min = 0),
          actionButton("predict_sanitation", "Predict Sanitation")
        )
      })
      
      # Hide all others
      output$nutrition_ui <- renderUI(NULL)
      output$health_ui <- renderUI(NULL)
      output$social_ui <- renderUI(NULL)
      
      showNotification("Sanitation model loaded.", type = "message")
    })
    
    observeEvent(input$predict_sanitation, {
      tryCatch({
        req(input$san_dw, input$san_du)
        
        # Validate numerics early
        if (is.na(input$san_dw) || is.na(input$san_du)) {
          showNotification(" Please enter numeric values for both weighted and unweighted denominators.", type = "error")
          return()
        }
        if (input$san_du == 0) {
          showNotification("Denominator Unweighted cannot be zero!", type = "error")
          return()
        }
        
        # Create data frame for prediction
        newdata_full <- data.frame(
          DenominatorWeighted = input$san_dw,
          DenominatorUnweighted = input$san_du
        )
        
        if ("IndicatorType" %in% names(template_df)) {
          req(input$IndicatorType)
          newdata_full$IndicatorType <- factor(input$IndicatorType, levels = IndicatorType_levels)
        }
        
        if ("CharacteristicCategory" %in% names(template_df)) {
          req(input$CharacteristicCategory)
          newdata_full$CharacteristicCategory <- factor(input$CharacteristicCategory, levels = CharacteristicCategory_levels)
        }
        
        # Add ratio
        newdata_full$Weight_Ratio <- newdata_full$DenominatorWeighted / newdata_full$DenominatorUnweighted
        
        # Try to encode with dummyVars safely
        X_new <- tryCatch({
          predict(dummies, newdata = newdata_full)
        }, error = function(e) {
          showNotification(paste("Encoding error:", e$message), type = "error")
          return(NULL)
        })
        
        if (is.null(X_new)) {
          return()
        }
        
        # Check feature alignment
        missing_feats <- setdiff(feature_names_san, colnames(X_new))
        extra_feats <- setdiff(colnames(X_new), feature_names_san)
        
        if (length(missing_feats) > 0) {
          showNotification(paste("Missing features in input:", paste(missing_feats, collapse = ", ")), type = "error")
          return()
        }
        if (length(extra_feats) > 0) {
          showNotification(paste("Extra unexpected features found:", paste(extra_feats, collapse = ", ")), type = "warning")
        }
        
        # Reorder columns
        X_new <- X_new[, feature_names_san, drop = FALSE]
        
        # Ensure all numeric
        if (!is.numeric(X_new)) {
          showNotification("Input data must be numeric after encoding.", type = "error")
          return()
        }
        
        # Predict safely
        pred <- tryCatch({
          dmat <- xgb.DMatrix(data = as.matrix(X_new))
          predict(xgb_sanitation, dmat)
        }, error = function(e) {
          showNotification(paste("Prediction error:", e$message), type = "error")
          return(NULL)
        })
        
        if (is.null(pred)) {
          showNotification("Prediction failed — check your inputs.", type = "error")
          return()
        }
        
        # Success modal
        showModal(modalDialog(
          title = "Sanitation Prediction",
          paste("Predicted Value:", round(pred, 2)),
          easyClose = TRUE
        ))
        
      }, error = function(e) {
        showNotification(paste("Unexpected error:", e$message), type = "error", duration = 8)
      })
    })
    # HEALTH MODEL
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
      newdata <- data.frame(Value = input$health_val, SurveyYear = input$health_year)
      
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
    })
    
    # SOCIAL MODEl
    observeEvent(input$show_social, {
      output$social_ui <- renderUI({
        tagList(
          h3("Social Model Inputs"),
          fluidRow(
            column(6,
                   selectInput("social_charcat", "Characteristic Category (1 = Male, 2 = Female):", choices = c("1", "2")),
                   helpText("Select gender for this record.")
            ),
            column(6,
                   numericInput("social_value", "Survey Year:", value = 2020, min = 2000, max = 2030),
                   helpText("Enter survey year.")
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
      req(input$social_charcat, input$social_value)
      newdata <- data.frame(
        CharacteristicCategory = factor(input$social_charcat, levels = c("1", "2")), 
        SurveyYear = input$social_value
      )
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
    })

  }
  shinyApp(ui, server)