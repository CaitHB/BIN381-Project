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
library(plotly)  


#Load models +  metadata

# Nutrition ( saved as a list)
  saved_nutrition <- readRDS("finalModels/rf_nutrition_model.rds")
rf_nutrition      <- saved_nutrition$model
feature_names_nut <- saved_nutrition$feature_names
levels_nut        <- saved_nutrition$levels  

    #nutrition dummies setup  

    # The model expects k-1 dummies with names like IndicatorTypeI
    # Set sep="" 

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


#helper for visuals

varimp_safe <- function(model) {
  vi <- try(caret::varImp(model), silent = TRUE)
  if (!inherits(vi, "try-error")) {
    if (is.list(vi) && !is.null(vi$importance)) {
      df <- vi$importance
      df$var <- rownames(df); rownames(df) <- NULL
      names(df)[1] <- "Overall"
      df <- df[, c("var", "Overall")]
    } else {
      df <- as.data.frame(vi)
      df$var <- rownames(df); rownames(df) <- NULL
      names(df)[1] <- "Overall"
      df <- df[, c("var", "Overall")]
    }
    return(df)
  }
  if (!is.null(model) && !is.null(model$importance)) {
    imp <- model$importance
    if (is.matrix(imp)) {
      df <- data.frame(var = rownames(imp), Overall = imp[, 1], row.names = NULL)
    } else {
      df <- data.frame(var = names(imp), Overall = as.numeric(imp), row.names = NULL)
    }
    return(df)
  }
  NULL
}





# UI mainpage

ui <- fluidPage(
  titlePanel("SocioHealth Analytics SA"),
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
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  
  
  #A  nutrition model
  nut_pred <- reactiveVal(NULL)
  
  observeEvent(input$show_nutrition, {
    indcat_choices <- levels_nut$IndicatorCategoryHigh %||% character(0)
    indtype_choices <- levels_nut$IndicatorType %||% character(0)
    chcat_choices   <- levels_nut$CharacteristicCategory %||% character(0)
    popgrp_choices  <- levels_nut$PopulationGroup %||% character(0)
    
    output$nutrition_ui <- renderUI({
      tagList(
        h3("Nutrition Model Inputs"),
        helpText("Provide inputs, then click Predict to see results & charts."),
        numericInput("nut_den_weight", "Denominator Weighted (DW):", value = 1000, min = 0),
        numericInput("nut_den_unweight", "Denominator Unweighted (DU):", value = 1000, min = 0),
        
        if (length(indcat_choices) > 0) {
          selectInput("nut_indcat", "Indicator Category High:", choices = indcat_choices)
        } else {
          helpText("IndicatorCategoryHigh levels not found in saved model metadata.")
        },
        
        if (length(indtype_choices) > 0) selectInput("nut_indicator_type", "Indicator Type:", choices = indtype_choices),
        if (length(chcat_choices)   > 0) selectInput("nut_characteristic_category", "Characteristic Category:", choices = chcat_choices),
        if (length(popgrp_choices)  > 0) selectInput("nut_population_group", "Population Group:", choices = popgrp_choices),
        
        numericInput("nut_survey_year", "Survey Year:", value = 2020, min = 2000, max = 2030),
        actionButton("predict_nutrition", "Predict Nutrition"),
        
        # ---- Appears AFTER prediction ----
        uiOutput("nut_after_ui")
      )
    })
    
    # hide others
    output$sanitation_ui <- renderUI(NULL)
    output$health_ui     <- renderUI(NULL)
    output$social_ui     <- renderUI(NULL)
    
    showNotification("Nutrition model loaded.", type = "message")
  })
  
  observeEvent(input$predict_nutrition, {
    req(input$nut_den_weight, input$nut_den_unweight, input$nut_indicator_type,
        input$nut_characteristic_category, input$nut_population_group,
        input$nut_survey_year, input$nut_indcat)
    
    nid <- showNotification("Running nutrition prediction…", type = "message", duration = NULL, closeButton = FALSE)
    on.exit(try(removeNotification(nid), silent = TRUE), add = TRUE)
    
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
    
    if ("Weight_Ratio" %in% req_nut_cols) {
      base_df$Weight_Ratio <- base_df$DenominatorWeighted / pmax(base_df$DenominatorUnweighted, 1e-9)
    }
    
    X_cat <- predict(dummies_nutrition, newdata = base_df) |> as.data.frame(check.names = FALSE)
    X_num <- base_df[, setdiff(names(base_df), c("IndicatorType","CharacteristicCategory","PopulationGroup","IndicatorCategoryHigh")), drop = FALSE]
    X_new <- cbind(X_num, X_cat)
    miss <- setdiff(req_nut_cols, names(X_new)); if (length(miss)) for (m in miss) X_new[[m]] <- 0
    extra <- setdiff(names(X_new), req_nut_cols); if (length(extra)) X_new <- X_new[, setdiff(names(X_new), extra), drop = FALSE]
    X_new <- X_new[, req_nut_cols, drop = FALSE]
    
    tryCatch({
      pred <- predict(rf_nutrition, newdata = X_new)
      nut_pred(as.numeric(pred))
      
      showModal(modalDialog(
        title = "Nutrition Prediction",
        paste("Predicted Nutrition Value:", round(nut_pred(), 2)),
        easyClose = TRUE
      ))
      # Build AFTER UI (interactive charts)
      output$nut_after_ui <- renderUI({
        req(nut_pred())
        tagList(
          hr(),
          h4("Interactive Charts"),
          fluidRow(
            column(6, plotlyOutput("nut_col_pred", height = "320px")),
            column(6, plotlyOutput("nut_pie_dwdu", height = "320px"))
          ),
          br(),
          plotlyOutput("nut_varimp_int", height = "380px")
        )
      })
      
      # Column chart with predicted value
      output$nut_col_pred <- renderPlotly({
        df <- data.frame(Metric = "Predicted Nutrition", Value = nut_pred())
        plot_ly(df, x = ~Metric, y = ~Value, type = "bar", text = ~round(Value,2), textposition = "auto") |>
          layout(yaxis = list(title = "Value"), xaxis = list(title = ""))
      })
      
      # Pie: DW vs DU (inputs you provided)
      output$nut_pie_dwdu <- renderPlotly({
        slices <- data.frame(
          Part = c("DW", "DU"),
          Amount = c(as.numeric(input$nut_den_weight), as.numeric(input$nut_den_unweight))
        )
        plot_ly(slices, labels = ~Part, values = ~Amount, type = "pie", textinfo = "label+percent") |>
          layout(title = "DW vs DU Composition")
      })
      
      # VarImp (interactive column)
      output$nut_varimp_int <- renderPlotly({
        df <- varimp_safe(rf_nutrition)
        req(!is.null(df), nrow(df) > 0)
        df <- df[order(df$Overall, decreasing = TRUE), ]
        top <- head(df, 20)
        plot_ly(top, x = ~reorder(var, Overall), y = ~Overall, type = "bar") |>
          layout(title = "Top Variable Importance", xaxis = list(title = ""), yaxis = list(title = "Importance")) |>
          layout(xaxis = list(tickangle = -45))
      })
      
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
  san_pred <- reactiveVal(NULL)
  
  # simple encoder skeleton to satisfy dummyVars (not used post-predict visuals)
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
        if (has_survey_year) numericInput("san_year", "Survey Year:", value = 2020, min = 1990, max = 2100),
        if (has_weight_ratio) tagList(
          numericInput("san_dw", "Denominator Weighted:", value = 1000, min = 0),
          numericInput("san_du", "Denominator Unweighted:", value = 1000, min = 1)
        ),
        if (length(indicator_feats))
          selectizeInput("san_ind", "IndicatorId* (select all that apply):",
                         choices = indicator_feats, multiple = TRUE),
        actionButton("predict_sanitation", "Predict Sanitation"),
        
        
        uiOutput("san_after_ui")
      )
    })
    output$nutrition_ui <- renderUI(NULL)
    output$health_ui    <- renderUI(NULL)
    output$social_ui    <- renderUI(NULL)
    showNotification("Sanitation model loaded.", type = "message", duration = 2)
  })
 
  observeEvent(input$predict_sanitation, {
    
    if (is.null(input$san_ind) || length(input$san_ind) == 0) {
      showNotification("Please select at least one IndicatorId* before predicting.", type = "error", duration = 5)
      return(invisible(NULL))
    }
    
    nid <- showNotification("Running sanitation prediction…", type = "message",
                            duration = NULL, closeButton = FALSE)
    on.exit(try(removeNotification(nid), silent = TRUE), add = TRUE)
    
    X <- setNames(as.list(rep(0, length(feature_names_san))), feature_names_san)
    
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
    
    sel <- input$san_ind %||% character(0)
    sel <- intersect(sel, feature_names_san)
    for (nm in sel) X[[nm]] <- 1
    
    if (length(dataset_feats)) {
      for (nm in dataset_feats) if (nm %in% names(X)) X[[nm]] <- 0
    }
    
    X_new <- as.matrix(as.data.frame(X, check.names = FALSE))[ , feature_names_san, drop = FALSE]
    
    tryCatch({
      dmat <- xgboost::xgb.DMatrix(data = X_new)
      pred <- predict(xgb_sanitation, dmat)
      san_pred(as.numeric(pred))
      
      showModal(modalDialog(
        title = "Sanitation Prediction",
        paste("Predicted Value:", round(san_pred(), 2)),
        easyClose = TRUE
      ))
      
      # AFTER UI: interactive charts
      output$san_after_ui <- renderUI({
        req(san_pred())
        tagList(
          hr(),
          h4("Interactive Charts"),
          fluidRow(
            column(6, plotlyOutput("san_col_pred", height = "320px")),
            column(6, plotlyOutput("san_pie_inds", height = "320px"))
          ),
          br(),
          plotlyOutput("san_importance_int", height = "380px")
        )
      })
      
      # Column (predicted)
      output$san_col_pred <- renderPlotly({
        df <- data.frame(Metric = "Predicted Sanitation", Value = san_pred())
        plot_ly(df, x = ~Metric, y = ~Value, type = "bar", text = ~round(Value,2), textposition = "auto") |>
          layout(yaxis = list(title = "Value"), xaxis = list(title = ""))
      })
      
      # Pie: Selected vs Unselected indicators
      output$san_pie_inds <- renderPlotly({
        total_inds <- length(indicator_feats)
        sel_count  <- length(input$san_ind %||% character(0))
        un_count   <- max(total_inds - sel_count, 0)
        slices <- data.frame(
          Part = c("Selected Indicators", "Unselected"),
          Amount = c(sel_count, un_count)
        )
        plot_ly(slices, labels = ~Part, values = ~Amount, type = "pie", textinfo = "label+percent") |>
          layout(title = "IndicatorId Selection")
      })
      
      # XGB importance 
      output$san_importance_int <- renderPlotly({
        imp <- tryCatch({
          xgboost::xgb.importance(model = xgb_sanitation)
        }, error = function(e) NULL)
        req(!is.null(imp), nrow(imp) > 0)
        imp <- imp[1:min(20, nrow(imp)), ]
        plot_ly(imp, x = ~reorder(Feature, Gain), y = ~Gain, type = "bar") |>
          layout(title = "Top XGBoost Feature Importance (Gain)",
                 xaxis = list(title = "", tickangle = -45),
                 yaxis = list(title = "Gain"))
      })
      
    }, error = function(e) {
      showNotification(paste0("Sanitation prediction failed: ", e$message,
                              "\nNeeded: [", paste(feature_names_san, collapse = ", "),
                              "]\nHave: [", paste(colnames(X_new), collapse = ", "), "]"),
                       type = "error", duration = 10)
    })
  })
  
  # C. Health model (logit)
  health_prob <- reactiveVal(NULL)
  health_class <- reactiveVal(NULL)
  
  observeEvent(input$show_health, {
    year_levels <- if (!is.null(logit_health$xlevels$SurveyYear)) logit_health$xlevels$SurveyYear else c("1998","2016")
    ind_levels  <- if (!is.null(logit_health$xlevels$IndicatorId)) logit_health$xlevels$IndicatorId else character(0)
    
    output$health_ui <- renderUI({
      tagList(
        h3("Health Model Inputs"),
        selectInput("health_year", "Survey Year:", choices = year_levels, selected = year_levels[1]),
        selectInput("health_ind",  "IndicatorId:", choices = ind_levels,  selected = ind_levels[1]),
        numericInput("health_dw",  "Denominator Weighted (DW):", value = 1000, min = 1),
        helpText("DW_log is computed as log(DW)."),
        actionButton("predict_health", "Predict Health"),
        
        uiOutput("health_after_ui")
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
    
    newdata <- data.frame(
      SurveyYear = factor(input$health_year, levels = logit_health$xlevels$SurveyYear),
      IndicatorId = factor(input$health_ind, levels = logit_health$xlevels$IndicatorId),
      DW_log = log(as.numeric(input$health_dw))
    )
    
    nid <- showNotification("Running health prediction…", type = "message", duration = NULL, closeButton = FALSE)
    on.exit(try(removeNotification(nid), silent = TRUE), add = TRUE)
    
    tryCatch({
      p <- as.numeric(predict(logit_health, newdata = newdata, type = "response"))
      cls <- ifelse(p >= 0.5, "High", "Low")
      health_prob(p)
      health_class(cls)
      
      showModal(modalDialog(
        title = "Health Prediction",
        paste0("Predicted Class: ", cls, " (Probability: ", sprintf("%.3f", p), ")"),
        easyClose = TRUE
      ))
      
      output$health_after_ui <- renderUI({
        req(health_prob(), health_class())
        tagList(
          hr(),
          h4("Interactive Charts"),
          fluidRow(
            column(6, plotlyOutput("health_pie_prob", height = "320px")),
            column(6, plotlyOutput("health_col_prob", height = "320px"))
          ),
          br(),
          plotlyOutput("health_coef_int", height = "420px")
        )
      })
      
      # Pie: Prob High vs Low
      output$health_pie_prob <- renderPlotly({
        slices <- data.frame(
          Class = c("High", "Low"),
          Prob  = c(health_prob(), 1 - health_prob())
        )
        plot_ly(slices, labels = ~Class, values = ~Prob, type = "pie", textinfo = "label+percent") |>
          layout(title = "Class Probabilities")
      })
      
      # Column: Probability bar with 0.5 threshold line
      output$health_col_prob <- renderPlotly({
        df <- data.frame(Measure = health_class(), Probability = health_prob())
        plt <- plot_ly(df, x = ~Measure, y = ~Probability, type = "bar",
                       text = ~sprintf("%.3f", Probability), textposition = "auto") |>
          layout(yaxis = list(title = "Probability", range = c(0,1)),
                 xaxis = list(title = ""),
                 shapes = list(list(type = "line", x0 = -0.5, x1 = 1.5, y0 = 0.5, y1 = 0.5,
                                    line = list(dash = "dot"))))
        plt
      })
      
      # Coefficients forest (interactive)
      output$health_coef_int <- renderPlotly({
        sm <- coef(summary(logit_health))
        df <- data.frame(
          term = rownames(sm),
          estimate = sm[, "Estimate"],
          se = sm[, "Std. Error"],
          stringsAsFactors = FALSE
        )
        df <- df[df$term != "(Intercept)", , drop = FALSE]
        df$lo <- df$estimate - 1.96 * df$se
        df$hi <- df$estimate + 1.96 * df$se
        plot_ly(df, x = ~estimate, y = ~term, type = "scatter", mode = "markers",
                error_x = list(array = 1.96 * df$se, thickness = 1)) |>
          layout(title = "Logistic Regression Coefficients",
                 xaxis = list(title = "β (95% CI)"), yaxis = list(title = ""))
      })
      
    }, error = function(e) {
      showNotification(paste("Health prediction error:", e$message), type = "error", duration = 8)
    })
  })
  
  
  #D  Social model
  
  social_pred <- reactiveVal(NULL)
  
  observeEvent(input$show_social, {
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
        actionButton("predict_social", "Predict Social"),
        
      
        uiOutput("social_after_ui")
      )
    })
    
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
    
    newdata <- data.frame(
      CharacteristicCategory = factor(input$social_charcat, levels = rf_social$xlevels$CharacteristicCategory),
      CharacteristicLabel    = factor(input$social_charlab, levels = rf_social$xlevels$CharacteristicLabel),
      IndicatorId            = factor(input$social_ind,    levels = rf_social$xlevels$IndicatorId),
      SurveyYear             = as.integer(input$social_year)
    )
    
    tryCatch({
      pred <- predict(rf_social, newdata = newdata)
      social_pred(as.numeric(pred))
      
      showModal(modalDialog(
        title = "Social Prediction",
        paste("Predicted Value:", round(social_pred(), 2)),
        easyClose = TRUE
      ))
      
      output$social_after_ui <- renderUI({
        req(social_pred())
        tagList(
          hr(),
          h4("Interactive Charts"),
          fluidRow(
            column(6, plotlyOutput("social_col_pred", height = "320px")),
            column(6, plotlyOutput("social_pie_factors", height = "320px"))
          ),
          br(),
          plotlyOutput("social_varimp_int", height = "380px")
        )
      })
      
      # Column: predicted
      output$social_col_pred <- renderPlotly({
        df <- data.frame(Metric = "Predicted Social", Value = social_pred())
        plot_ly(df, x = ~Metric, y = ~Value, type = "bar", text = ~round(Value,2), textposition = "auto") |>
          layout(yaxis = list(title = "Value"), xaxis = list(title = ""))
      })
      
      # Pie: categorical selections snapshot
      output$social_pie_factors <- renderPlotly({
        # Show how inputs split across three categorical choices (just a visual snapshot)
        slices <- data.frame(
          Part = c("CharacteristicCategory", "CharacteristicLabel", "IndicatorId"),
          Count = c(1,1,1)
        )
        plot_ly(slices, labels = ~Part, values = ~Count, type = "pie", textinfo = "label+percent") |>
          layout(title = "Selected Factor Snapshot")
      })
      
      # VarImp AFTER prediction
      output$social_varimp_int <- renderPlotly({
        df <- varimp_safe(rf_social)
        req(!is.null(df), nrow(df) > 0)
        df <- df[order(df$Overall, decreasing = TRUE), ]
        top <- head(df, 20)
        plot_ly(top, x = ~reorder(var, Overall), y = ~Overall, type = "bar") |>
          layout(title = "Top Variable Importance",
                 xaxis = list(title = "", tickangle = -45),
                 yaxis = list(title = "Importance"))
      })
      
    }, error = function(e) {
      showNotification(paste("Social prediction error:", e$message), type = "error", duration = 8)
    })
  })
}

shinyApp(ui, server)