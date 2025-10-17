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
library(randomForest)
library(xgboost) 

#Load models +  metadata

# Nutrition ( saved as a list)
  saved_nutrition <- readRDS("finalModels/rf_nutrition_model.rds")
rf_nutrition      <- saved_nutrition$model
feature_names_nut <- saved_nutrition$feature_names
levels_nut        <- saved_nutrition$levels  

    #nutrition dumies setup  

    # The model expects k-1 dummies (reference level dropped) with names like "IndicatorTypeI"
    # Set sep="" so we get "IndicatorTypeI" instead of "IndicatorType.I".

    nut_template <- data.frame(
      IndicatorType          = factor(levels_nut$IndicatorType[1],          levels = levels_nut$IndicatorType),
      CharacteristicCategory = factor(levels_nut$CharacteristicCategory[1], levels = levels_nut$CharacteristicCategory),
      PopulationGroup        = factor(levels_nut$PopulationGroup[1],        levels = levels_nut$PopulationGroup),
      IndicatorCategoryHigh  = factor(levels_nut$IndicatorCategoryHigh[1],  levels = levels_nut$IndicatorCategoryHigh)
    )

    dummies_nutrition <- caret::dummyVars(
      ~ IndicatorType + CharacteristicCategory + PopulationGroup + IndicatorCategoryHigh,
      data     = nut_template,
      fullRank = TRUE,   # k-1 columns (drops first level)
      sep      = ""      # "IndicatorTypeI" not "IndicatorType.I"
    )

    # Columns the RF wants
    req_nut_cols <- rf_nutrition$xNames

# Sanitation (XGB)
feature_names_san <- readRDS("finalModels/xgb_sanitation_feature_names.rds")
dataset_feats <- grep("^Dataset(Toilet|Water)_", feature_names_san, value = TRUE)
indicator_feats <- grep("^IndicatorId", feature_names_san, value = TRUE)

has_survey_year  <- "SurveyYear"   %in% feature_names_san
has_weight_ratio <- "Weight_Ratio" %in% feature_names_san
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


  
# Server
server <- function(input, output, session) {
  
  # helper
  get_levels <- function(model, var) {
    if (!is.null(model$xlevels) && !is.null(model$xlevels[[var]])) return(model$xlevels[[var]])
    return(NULL)
  }
  
  
  #A  nutrition model
  observeEvent(input$show_nutrition, {
    # fallback choices if any levels are missing
    indcat_choices <- if (!is.null(levels_nut$IndicatorCategoryHigh)) levels_nut$IndicatorCategoryHigh else character(0)
    indtype_choices <- if (!is.null(levels_nut$IndicatorType)) levels_nut$IndicatorType else character(0)
    chcat_choices   <- if (!is.null(levels_nut$CharacteristicCategory)) levels_nut$CharacteristicCategory else character(0)
    popgrp_choices  <- if (!is.null(levels_nut$PopulationGroup)) levels_nut$PopulationGroup else character(0)
    
    output$nutrition_ui <- renderUI({
      tagList(
        h3("Nutrition Model Inputs"),
        helpText("Fill in the following features to predict Nutrition Value:"),
        numericInput("nut_den_weight", "Denominator Weighted (DW):", value = 1000, min = 0),
        numericInput("nut_den_unweight", "Denominator Unweighted (DU):", value = 1000, min = 0),
        
        if (length(indcat_choices) > 0) {
          selectInput("nut_indcat", "Indicator Category High:", choices = indcat_choices)
        } else {
          helpText("⚠ IndicatorCategoryHigh levels not found in saved model metadata.")
        },
        
        if (length(indtype_choices) > 0) selectInput("nut_indicator_type", "Indicator Type:", choices = indtype_choices),
        if (length(chcat_choices)   > 0) selectInput("nut_characteristic_category", "Characteristic Category:", choices = chcat_choices),
        if (length(popgrp_choices)  > 0) selectInput("nut_population_group", "Population Group:", choices = popgrp_choices),
        
        numericInput("nut_survey_year", "Survey Year:", value = 2020, min = 2000, max = 2030),
        actionButton("predict_nutrition", "Predict Nutrition")
      )
    })
    
    # hide other panels when Nutrition is shown
    output$sanitation_ui <- renderUI(NULL)
    output$health_ui     <- renderUI(NULL)
    output$social_ui     <- renderUI(NULL)
    
    showNotification("Nutrition model loaded.", type = "message")
  }) 
  
  observeEvent(input$predict_nutrition, {
    req(input$nut_den_weight, input$nut_den_unweight, input$nut_indicator_type,
        input$nut_characteristic_category, input$nut_population_group,
        input$nut_survey_year, input$nut_indcat)
    
    # in-flight notice (store id so we can remove it)
    nid <- showNotification("Running nutrition prediction…", type = "message", duration = NULL, closeButton = FALSE)
    
    on.exit({
     
      try(removeNotification(nid), silent = TRUE)
    }, add = TRUE)
    
    # build base_df 
    base_df <- data.frame(
      IndicatorType          = factor(input$nut_indicator_type,          levels = levels_nut$IndicatorType),
      CharacteristicCategory = factor(input$nut_characteristic_category, levels = levels_nut$CharacteristicCategory),
      PopulationGroup        = factor(input$nut_population_group,        levels = levels_nut$PopulationGroup),
      IndicatorCategoryHigh  = factor(input$nut_indcat,                  levels = levels_nut$IndicatorCategoryHigh),
      DenominatorWeighted    = as.numeric(input$nut_den_weight),
      DenominatorUnweighted  = as.numeric(input$nut_den_unweight),
      SurveyYear             = as.numeric(input$nut_survey_year),
      stringsAsFactors       = FALSE
    )
    
    # the RF expects dummies 
    if (!exists("req_nut_cols", inherits = TRUE)) req_nut_cols <- rf_nutrition$xNames
    
    if ("Weight_Ratio" %in% req_nut_cols) {
      base_df$Weight_Ratio <- base_df$DenominatorWeighted / pmax(base_df$DenominatorUnweighted, 1e-9)
    }
    
    # build dummies with the encoder 
    X_cat <- predict(dummies_nutrition, newdata = base_df) |> as.data.frame(check.names = FALSE)
    X_num <- base_df[, setdiff(names(base_df), c("IndicatorType","CharacteristicCategory","PopulationGroup","IndicatorCategoryHigh")), drop = FALSE]
    X_new <- cbind(X_num, X_cat)
    
    miss <- setdiff(req_nut_cols, names(X_new)); if (length(miss)) for (m in miss) X_new[[m]] <- 0
    extra <- setdiff(names(X_new), req_nut_cols); if (length(extra)) X_new <- X_new[, setdiff(names(X_new), extra), drop = FALSE]
    X_new <- X_new[, req_nut_cols, drop = FALSE]
    
    tryCatch({
      pred <- predict(rf_nutrition, newdata = X_new)
      
      # success
      showModal(modalDialog(
        title = "Nutrition Prediction",
        paste("Predicted Nutrition Value:", round(as.numeric(pred), 2)),
        easyClose = TRUE
      ))
    }, error = function(e) {
   
      need <- paste(req_nut_cols, collapse = ", ")
      have <- paste(colnames(X_new), collapse = ", ")
      showNotification(paste0("Nutrition prediction failed: ", e$message,
                              "\nNeeded: [", need, "]\nHave: [", have, "]"),
                       type = "error", duration = 10)
    })
  })
  
# B. sanitation modelL
  
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
        
        if (has_survey_year)
          numericInput("san_year", "Survey Year:", value = 2020, min = 1990, max = 2100),
        
        if (has_weight_ratio) tagList(
          numericInput("san_dw", "Denominator Weighted:", value = 1000, min = 0),
          numericInput("san_du", "Denominator Unweighted:", value = 1000, min = 1)
        ),
        
       
        if (length(indicator_feats))
          selectizeInput("san_ind", "IndicatorId* (select all that apply):",
                         choices = indicator_feats, multiple = TRUE),
        
        actionButton("predict_sanitation", "Predict Sanitation")
      )
    })
    output$nutrition_ui <- renderUI(NULL)
    output$health_ui    <- renderUI(NULL)
    output$social_ui    <- renderUI(NULL)
    showNotification("Sanitation model loaded.", type = "message", duration = 2)
  })
  
  #  Predict (auto-zeros Dataset* flags, aligns, predicts)
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  observeEvent(input$predict_sanitation, {
   
    nid <- showNotification("Running sanitation prediction…", type = "message",
                            duration = NULL, closeButton = FALSE)
    on.exit(try(removeNotification(nid), silent = TRUE), add = TRUE)
    
    # Start with zero for ALL expected features (length = 105)
    X <- setNames(as.list(rep(0, length(feature_names_san))), feature_names_san)
    
    # SurveyYear / Weight_Ratio if present 
    if (has_survey_year) {
      req(input$san_year)
      X[["SurveyYear"]] <- as.numeric(input$san_year)
    }
    if (has_weight_ratio) {
      req(input$san_dw, input$san_du)
      validate(need(is.finite(input$san_dw) && is.finite(input$san_du) && input$san_du > 0,
                    "Enter valid denominators; Unweighted must be > 0."))
      X[["Weight_Ratio"]] <- as.numeric(input$san_dw) / as.numeric(input$san_du)
    }
    
    # Flip selected IndicatorId* flags to 1
    sel <- input$san_ind %||% character(0)
    sel <- intersect(sel, feature_names_san)
    for (nm in sel) X[[nm]] <- 1
    
    #  zero out any Dataset* flags 
    if (length(dataset_feats)) {
      for (nm in dataset_feats) if (nm %in% names(X)) X[[nm]] <- 0
    }
    
    #  matrix in the order the booster expects
    X_new <- as.matrix(as.data.frame(X, check.names = FALSE))[ , feature_names_san, drop = FALSE]
    
    # Predict
    tryCatch({
      dmat <- xgboost::xgb.DMatrix(data = X_new)
      pred <- predict(xgb_sanitation, dmat)
      showModal(modalDialog(
        title = "Sanitation Prediction",
        paste("Predicted Value:", round(as.numeric(pred), 2)),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showNotification(paste0("Sanitation prediction failed: ", e$message,
                              "\nNeeded: [", paste(feature_names_san, collapse = ", "),
                              "]\nHave: [", paste(colnames(X_new), collapse = ", "), "]"),
                       type = "error", duration = 10)
    })
  })
  
  # C. Health model (logit)
  observeEvent(input$show_health, {
    # pull levels from the model
    year_levels <- if (!is.null(logit_health$xlevels$SurveyYear)) logit_health$xlevels$SurveyYear else c("1998","2016")
    ind_levels  <- if (!is.null(logit_health$xlevels$IndicatorId)) logit_health$xlevels$IndicatorId else character(0)
    
    output$health_ui <- renderUI({
      tagList(
        h3("Health Model Inputs"),
        selectInput("health_year", "Survey Year:", choices = year_levels, selected = year_levels[1]),
        selectInput("health_ind",  "IndicatorId:", choices = ind_levels,  selected = ind_levels[1]),
        numericInput("health_dw",  "Denominator Weighted (DW):", value = 1000, min = 1),
        helpText("DW_log will be computed as log(DW)."),
        actionButton("predict_health", "Predict Health")
      )
    })
    output$nutrition_ui <- renderUI(NULL)
    output$sanitation_ui <- renderUI(NULL)
    output$social_ui <- renderUI(NULL)
    showNotification("Health model loaded.", type = "message")
  })
  
  observeEvent(input$predict_health, {
    req(input$health_year, input$health_ind, input$health_dw)
    validate(need(is.finite(input$health_dw) && input$health_dw > 0, "DW must be > 0"))
    
    # DW_log as used in training (R's log() is natural log)
    DW_log_val <- log(as.numeric(input$health_dw))
    
    newdata <- data.frame(
      SurveyYear = factor(input$health_year, levels = logit_health$xlevels$SurveyYear),
      IndicatorId = factor(input$health_ind, levels = logit_health$xlevels$IndicatorId),
      DW_log = DW_log_val
    )
    
    # in-flight banner
    nid <- showNotification("Running health prediction…", type = "message", duration = NULL, closeButton = FALSE)
    on.exit(try(removeNotification(nid), silent = TRUE), add = TRUE)
    
    tryCatch({
      p <- predict(logit_health, newdata = newdata, type = "response")
      cls <- ifelse(p >= 0.5, "High", "Low")
      showModal(modalDialog(
        title = "Health Prediction",
        paste0("Predicted Class: ", cls, " (Probability: ", sprintf("%.3f", as.numeric(p)), ")"),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showNotification(paste("Health prediction error:", e$message), type = "error", duration = 8)
    })
  })
    
  
  #D  Social model
  
  observeEvent(input$show_social, {
    # pull training levels
    social_cat_levels  <- rf_social$xlevels$CharacteristicCategory
    social_lab_levels  <- rf_social$xlevels$CharacteristicLabel
    social_ind_levels  <- rf_social$xlevels$IndicatorId
    
    output$social_ui <- renderUI({
      tagList(
        h3("Social Model Inputs"),
        selectInput("social_charcat", "Characteristic Category:",
                    choices = social_cat_levels, selected = social_cat_levels[1]),
        selectInput("social_charlab", "Characteristic Label:",
                    choices = social_lab_levels, selected = social_lab_levels[1]),
        selectInput("social_ind", "IndicatorId:",
                    choices = social_ind_levels, selected = social_ind_levels[1]),
        numericInput("social_year", "Survey Year:", value = 2016, min = 1900, max = 2100),
        actionButton("predict_social", "Predict Social")
      )
    })
    
    # hide others
    output$nutrition_ui <- renderUI(NULL)
    output$sanitation_ui <- renderUI(NULL)
    output$health_ui    <- renderUI(NULL)
    
    showNotification("Social model loaded.", type = "message", duration = 2)
  })
  
  
  observeEvent(input$predict_social, {
    req(input$social_charcat, input$social_charlab, input$social_ind, input$social_year)
    
    
    nid <- showNotification("Running social prediction…", type = "message",
                            duration = NULL, closeButton = FALSE)
    on.exit(try(removeNotification(nid), silent = TRUE), add = TRUE)
    
    # build newdata with the model’s exact factor levels
    newdata <- data.frame(
      CharacteristicCategory = factor(input$social_charcat, levels = rf_social$xlevels$CharacteristicCategory),
      CharacteristicLabel    = factor(input$social_charlab, levels = rf_social$xlevels$CharacteristicLabel),
      IndicatorId            = factor(input$social_ind,    levels = rf_social$xlevels$IndicatorId),
      SurveyYear             = as.integer(input$social_year)
    )
    
    tryCatch({
      pred <- predict(rf_social, newdata = newdata)
      showModal(modalDialog(
        title = "Social Prediction",
        paste("Predicted Value:", round(as.numeric(pred), 2)),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showNotification(paste("Social prediction error:", e$message), type = "error", duration = 8)
    })
  })
  
}
shinyApp(ui, server)