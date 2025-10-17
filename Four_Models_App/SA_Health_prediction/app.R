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
library(xgboost) 

#Load models & metadata

# Nutrition ( saved as a list)
saved_nutrition <- readRDS("finalModels/rf_nutrition_model.rds")
rf_nutrition      <- saved_nutrition$model
feature_names_nut <- saved_nutrition$feature_names
levels_nut        <- saved_nutrition$levels  

# Sanitation (XGB)
feature_names_san <- readRDS("finalModels/xgb_sanitation_feature_names.rds")
xgb_sanitation    <- readRDS("finalModels/xgb_sanitation_model.rds")

# Health -logit
logit_health <- readRDS("finalModels/logit_healthA_model.rds")

# Social 
rf_social <- readRDS("finalModels/rf_model_GroupD_Social.rds")



# UI mainpage

ui <- fluidPage(
  titlePanel("Four Models Prediction App"),
  fluidRow(
    column(3, actionButton("show_nutrition", "Nutrition Model", width = "100%"),
           style = "background-color: #E6F2FF; padding: 20px; text-align:center;"),
    column(3, actionButton("show_sanitation", "Sanitation Model", width = "100%"),
           style = "background-color: #FFE6E6; padding: 20px; text-align:center;"),
    column(3, actionButton("show_health", "Health Model", width = "100%"),
           style = "background-color: #E6FFE6; padding: 20px; text-align:center;"),
    column(3, actionButton("show_social", "Social Model", width = "100%"),
           style = "background-color: #FFF5E6; padding: 20px; text-align:center;")
  ),
  hr(),
  uiOutput("nutrition_ui"),
  uiOutput("sanitation_ui"),
  uiOutput("health_ui"),
  uiOutput("social_ui")
)



# server
server <- function(input, output, session) {
  
  # helper
  get_levels <- function(model, var) {
    if (!is.null(model$xlevels) && !is.null(model$xlevels[[var]])) return(model$xlevels[[var]])
    return(NULL)
  }
  
  
  # nutrition model
  observeEvent(input$show_nutrition, {
    output$nutrition_ui <- renderUI({
      tagList(
        h3("Nutrition Model Inputs"),
        helpText("Fill in the following features to predict Nutrition Value:"),
        numericInput("nut_den_weight", "Denominator Weighted (DW):", value = 1000, min = 0),
        numericInput("nut_den_unweight", "Denominator Unweighted (DU):", value = 1000, min = 0),
        
        if (!is.null(levels_nut$IndicatorCategoryHigh) && length(levels_nut$IndicatorCategoryHigh) > 0) {
          selectInput("nut_indcat", "Indicator Category High:", choices = levels_nut$IndicatorCategoryHigh)
        } else {
          helpText("Error: Indicator categories not available. Please check model data.")
        },
        
        selectInput("nut_indicator_type", "Indicator Type:", choices = levels_nut$IndicatorType),
        selectInput("nut_characteristic_category", "Characteristic Category:", choices = levels_nut$CharacteristicCategory),
        selectInput("nut_population_group", "Population Group:", choices = levels_nut$PopulationGroup),
        numericInput("nut_survey_year", "Survey Year:", value = 2020, min = 2000, max = 2030),
        actionButton("predict_nutrition", "Predict Nutrition")
      )
    })
    output$sanitation_ui <- renderUI(NULL)
    output$health_ui <- renderUI(NULL)
    output$social_ui <- renderUI(NULL)
    showNotification("Nutrition model loaded.", type = "message")
  })
  
  observeEvent(input$predict_nutrition, {
    req(input$nut_den_weight, input$nut_den_unweight, input$nut_indicator_type,
        input$nut_characteristic_category, input$nut_population_group,
        input$nut_survey_year, input$nut_indcat)
    
    newdata <- data.frame(
      IndicatorType          = factor(input$nut_indicator_type,       levels = levels_nut$IndicatorType),
      DenominatorWeighted    = as.numeric(input$nut_den_weight),
      DenominatorUnweighted  = as.numeric(input$nut_den_unweight),
      CharacteristicCategory = factor(input$nut_characteristic_category, levels = levels_nut$CharacteristicCategory),
      PopulationGroup        = factor(input$nut_population_group,     levels = levels_nut$PopulationGroup),
      SurveyYear             = as.numeric(input$nut_survey_year),
      IndicatorCategoryHigh  = factor(input$nut_indcat,               levels = levels_nut$IndicatorCategoryHigh)
    )
    
    # Keep only features model expects
    if (!is.null(feature_names_nut)) {
      missing <- setdiff(feature_names_nut, names(newdata))
      if (length(missing)) {
        showNotification(paste("Nutrition: missing required features:", paste(missing, collapse = ", ")), type = "error")
        return()
      }
      newdata <- newdata[, feature_names_nut, drop = FALSE]
    }
    
    tryCatch({
      pred <- predict(rf_nutrition, newdata)
      showModal(modalDialog(
        title = "Nutrition Prediction",
        paste("Predicted Nutrition Value:", round(as.numeric(pred), 2)),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showNotification(paste("Nutrition prediction failed:", e$message), type = "error", duration = 8)
    })
  })
  
  
  
  #sanitation modelL
  
  # create a dummyVars encoder
  # Minimal template includes all possible factor levels 
  IndicatorType_levels <- c("I", "S", "T", "D", "U")      
  CharacteristicCategory_levels <- c("Total")             
  template_df <- data.frame(
    IndicatorType = factor(IndicatorType_levels, levels = IndicatorType_levels),
    CharacteristicCategory = factor(CharacteristicCategory_levels, levels = CharacteristicCategory_levels)
  )
  
  template_df$DenominatorWeighted <- 0
  template_df$DenominatorUnweighted <- 0
  template_df$Weight_Ratio <- 0
  dummies_san <- dummyVars(~ ., data = template_df)
  
  observeEvent(input$show_sanitation, {
    output$sanitation_ui <- renderUI({
      tagList(
        h3("Sanitation Model Inputs"),
        selectInput("IndicatorType", "Indicator Type", choices = IndicatorType_levels),
        selectInput("CharacteristicCategory", "Characteristic Category", choices = CharacteristicCategory_levels),
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
    tryCatch({
      req(input$san_dw, input$san_du, input$IndicatorType, input$CharacteristicCategory)
      if (is.na(input$san_dw) || is.na(input$san_du)) {
        showNotification("Enter numeric values for both denominators.", type = "error"); return()
      }
      if (input$san_du == 0) {
        showNotification("Denominator Unweighted cannot be zero.", type = "error"); return()
      }
      
      newdata_full <- data.frame(
        IndicatorType = factor(input$IndicatorType, levels = IndicatorType_levels),
        CharacteristicCategory = factor(input$CharacteristicCategory, levels = CharacteristicCategory_levels),
        DenominatorWeighted = as.numeric(input$san_dw),
        DenominatorUnweighted = as.numeric(input$san_du)
      )
      newdata_full$Weight_Ratio <- newdata_full$DenominatorWeighted / newdata_full$DenominatorUnweighted
      
      X_new <- tryCatch({
        predict(dummies_san, newdata = newdata_full) %>% as.data.frame()
      }, error = function(e) {
        showNotification(paste("Encoding error:", e$message), type = "error"); return(NULL)
      })
      if (is.null(X_new)) return()
      
      # align to training feature set
      missing_feats <- setdiff(feature_names_san, colnames(X_new))
      if (length(missing_feats) > 0) {
        # add missing columns as 0 
        for (m in missing_feats) X_new[[m]] <- 0
      }
      extra_feats <- setdiff(colnames(X_new), feature_names_san)
      if (length(extra_feats) > 0) {
        X_new <- X_new[, setdiff(colnames(X_new), extra_feats), drop = FALSE]
      }
      X_new <- X_new[, feature_names_san, drop = FALSE]
      
      dmat <- xgb.DMatrix(data = as.matrix(X_new))
      pred <- predict(xgb_sanitation, dmat)
      
      showModal(modalDialog(
        title = "Sanitation Prediction",
        paste("Predicted Value:", round(as.numeric(pred), 2)),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showNotification(paste("Sanitation: unexpected error:", e$message), type = "error", duration = 8)
    })
  })
  
  # Health model (logit)
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
    newdata <- data.frame(Value = as.numeric(input$health_val),
                          SurveyYear = as.numeric(input$health_year))
    
    # for (v in names(logit_health$xlevels)) newdata[[v]] <- factor(newdata[[v]], levels = logit_health$xlevels[[v]])
    
    tryCatch({
      pred_prob  <- predict(logit_health, newdata, type = "response")
      pred_class <- ifelse(pred_prob >= 0.5, "High", "Low")
      showModal(modalDialog(
        title = "Health Prediction",
        paste("Predicted Class:", pred_class, "(Probability:", round(as.numeric(pred_prob), 2), ")"),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showNotification(paste("Health prediction error:", e$message), type = "error")
    })
  })
  
  
  # Social model
  observeEvent(input$show_social, {
    
    social_char_levels <- get_levels(rf_social, "CharacteristicCategory")
    if (is.null(social_char_levels)) social_char_levels <- c("1","2")  # fallback
    
    output$social_ui <- renderUI({
      tagList(
        h3("Social Model Inputs"),
        fluidRow(
          column(6,
                 selectInput("social_charcat", "Characteristic Category:", choices = social_char_levels),
                 helpText("Pick the category level used in training (e.g., Male/Female or 1/2).")
          ),
          column(6,
                 numericInput("social_year", "Survey Year:", value = 2020, min = 2000, max = 2030),
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
    req(input$social_charcat, input$social_year)
    
    
    social_char_levels <- get_levels(rf_social, "CharacteristicCategory")
    if (is.null(social_char_levels)) social_char_levels <- c("1","2")
    
    newdata <- data.frame(
      CharacteristicCategory = factor(input$social_charcat, levels = social_char_levels),
      SurveyYear = as.numeric(input$social_year)
    )
    
    tryCatch({
      pred <- predict(rf_social, newdata)
      showModal(modalDialog(
        title = "Social Prediction",
        paste("Predicted Value:", round(as.numeric(pred), 2)),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showNotification(paste("Social prediction error:", e$message), type = "error")
    })
  })
}

shinyApp(ui, server)
