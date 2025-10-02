---
title: "Modelling – Dataset A (Dean)"
author: "Group K"
date: "2025-10-02"
output:
  pdf_document: default
  word_document: default
---





```
## Rows: 633 Columns: 10
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr (5): Dataset, CharacteristicCategory, CharacteristicLabel, IndicatorId, ...
## dbl (5): SurveyYear, CharacteristicId, Value, DenominatorWeighted, Denominat...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```



Table: First few rows of Dataset A (Health)

|Dataset                 | SurveyYear| CharacteristicId|CharacteristicCategory |CharacteristicLabel |IndicatorId   |IndicatorType | Value| DenominatorWeighted| DenominatorUnweighted|
|:-----------------------|----------:|----------------:|:----------------------|:-------------------|:-------------|:-------------|-----:|-------------------:|---------------------:|
|Access_To_Healthcare_01 |       1998|             1000|Total                  |Total               |RH_ANCP_W_DOC |I             |  28.5|                2871|                  2903|
|Access_To_Healthcare_01 |       1998|             1000|Total                  |Total               |RH_ANCP_W_DOC |I             |  30.0|                4122|                  4148|
|Access_To_Healthcare_01 |       1998|             1000|Total                  |Total               |RH_ANCP_W_DOC |I             |  27.3|                2010|                  2041|
|Access_To_Healthcare_01 |       1998|             1000|Total                  |Total               |RH_ANCP_W_NRS |I             |  66.6|                2871|                  2903|
|Access_To_Healthcare_01 |       1998|             1000|Total                  |Total               |RH_ANCP_W_NRS |I             |  65.0|                4122|                  4148|
|Access_To_Healthcare_01 |       1998|             1000|Total                  |Total               |RH_ANCP_W_NRS |I             |  68.4|                2010|                  2041|



Table: Filtered Health (I) – head()

|Dataset                 |SurveyYear | CharacteristicId|CharacteristicCategory |CharacteristicLabel |IndicatorId   |IndicatorType | Value| DenominatorWeighted| DenominatorUnweighted|
|:-----------------------|:----------|----------------:|:----------------------|:-------------------|:-------------|:-------------|-----:|-------------------:|---------------------:|
|Access_To_Healthcare_01 |1998       |             1000|Total                  |Total               |RH_ANCP_W_DOC |I             |  28.5|                2871|                  2903|
|Access_To_Healthcare_01 |1998       |             1000|Total                  |Total               |RH_ANCP_W_DOC |I             |  30.0|                4122|                  4148|
|Access_To_Healthcare_01 |1998       |             1000|Total                  |Total               |RH_ANCP_W_DOC |I             |  27.3|                2010|                  2041|
|Access_To_Healthcare_01 |1998       |             1000|Total                  |Total               |RH_ANCP_W_NRS |I             |  66.6|                2871|                  2903|
|Access_To_Healthcare_01 |1998       |             1000|Total                  |Total               |RH_ANCP_W_NRS |I             |  65.0|                4122|                  4148|
|Access_To_Healthcare_01 |1998       |             1000|Total                  |Total               |RH_ANCP_W_NRS |I             |  68.4|                2010|                  2041|




``` r
set.seed(42)
idx <- caret::createDataPartition(model_df$HighValue, p = 0.7, list = FALSE)
train_df <- model_df[idx, ]  %>% mutate(
  SurveyYear  = forcats::fct_drop(SurveyYear),
  IndicatorId = forcats::fct_drop(IndicatorId)
)
test_df  <- model_df[-idx, ] %>% mutate(
  SurveyYear  = factor(SurveyYear,  levels = levels(train_df$SurveyYear)),
  IndicatorId = factor(IndicatorId, levels = levels(train_df$IndicatorId))
)
```


```
## Setting direction: controls < cases
```

```
## pdf 
##   2
```



Table: Logistic Regression – Overall Metrics (Test)

| Accuracy| Kappa| AccuracyLower| AccuracyUpper| AccuracyNull| AccuracyPValue| McnemarPValue|
|--------:|-----:|-------------:|-------------:|------------:|--------------:|-------------:|
|    0.667| 0.333|         0.516|         0.796|          0.5|          0.015|         0.803|



Table: Logistic Regression – Class Metrics (Test)

| Sensitivity| Specificity| Pos Pred Value| Neg Pred Value| Precision| Recall|   F1| Prevalence| Detection Rate| Detection Prevalence| Balanced Accuracy|
|-----------:|-----------:|--------------:|--------------:|---------:|------:|----:|----------:|--------------:|--------------------:|-----------------:|
|       0.708|       0.625|          0.654|          0.682|     0.654|  0.708| 0.68|        0.5|          0.354|                0.542|             0.667|


```
## Setting direction: controls < cases
```

```
## pdf 
##   2
```



Table: Decision Tree – Overall Metrics (Test)

| Accuracy| Kappa| AccuracyLower| AccuracyUpper| AccuracyNull| AccuracyPValue| McnemarPValue|
|--------:|-----:|-------------:|-------------:|------------:|--------------:|-------------:|
|    0.708| 0.417|         0.559|          0.83|          0.5|          0.003|         0.423|



Table: Decision Tree – Class Metrics (Test)

| Sensitivity| Specificity| Pos Pred Value| Neg Pred Value| Precision| Recall|    F1| Prevalence| Detection Rate| Detection Prevalence| Balanced Accuracy|
|-----------:|-----------:|--------------:|--------------:|---------:|------:|-----:|----------:|--------------:|--------------------:|-----------------:|
|       0.792|       0.625|          0.679|           0.75|     0.679|  0.792| 0.731|        0.5|          0.396|                0.583|             0.708|



Table: Decision Tree – Top 10 Feature Importance

|            |   Overall|Feature     |
|:-----------|---------:|:-----------|
|IndicatorId | 15.976471|IndicatorId |
|DW_log      | 11.799693|DW_log      |
|SurveyYear  |  0.704405|SurveyYear  |


Table: Model Performance Comparison (Test Set)

|Model               | Accuracy| Kappa| Sensitivity| Specificity|   AUC|
|:-------------------|--------:|-----:|-----------:|-----------:|-----:|
|Logistic Regression |    0.667| 0.333|       0.708|       0.625| 0.832|
|Decision Tree       |    0.708| 0.417|       0.792|       0.625| 0.786|



```
## **Summary:** We trained two models on Dataset A’s health indicators (binary target: HighValue ≥ median(Value)). Both achieved reasonable discrimination. The comparison table above shows test-set metrics. Based on AUC (primary) and Accuracy (secondary), the better model in this run is: **Logistic Regression** (AUC = 0.832, Accuracy = 0.667). We also saved ROC curves, tree plot, metrics, and variable importance in ../outputs/ .
```
