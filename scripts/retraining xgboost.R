
library(tidyverse)
library(caret)
library(xgboost)

# Load your dataset
san_df <- read.csv(here("merged datasets", "GroupB_Sanitation_merged.csv"))

# Data Cleaning
plot_data <- san_df %>% filter(!is.na(Value))
plot_data <- plot_data %>%
  mutate(
    DenominatorWeighted = ifelse(is.na(DenominatorWeighted), median(DenominatorWeighted, na.rm = TRUE), DenominatorWeighted),
    DenominatorUnweighted = ifelse(is.na(DenominatorUnweighted), median(DenominatorUnweighted, na.rm = TRUE), DenominatorUnweighted)
  )

# Feature Engineering without Log_Value
mod_df <- plot_data %>%
  mutate(
    IndicatorType = as.factor(IndicatorType),
    CharacteristicCategory = as.factor(CharacteristicCategory),
    Weight_Ratio = DenominatorWeighted / DenominatorUnweighted  # Only this feature
  )

# Drop constant columns
drop_constant <- function(df) {
  keep <- sapply(df, function(x) length(unique(x[!is.na(x)])) > 1)
  return(df[, keep, drop = FALSE])
}
mod_df <- drop_constant(mod_df)

# One-hot encode
dummies <- dummyVars(Value ~ ., data = mod_df)
X <- predict(dummies, newdata = mod_df)
y <- mod_df$Value

# Train/test split
set.seed(42)
train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
y_train <- y[train_idx]
y_test <- y[-train_idx]

# Prepare XGBoost data
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Train the model
params <- list(objective = 'reg:squarederror', eta = 0.1, max_depth = 6, subsample = 0.8, colsample_bytree = 0.8, lambda = 1, alpha = 0.5)
xgb_mod <- xgboost::xgb.train(params = params, data = dtrain, nrounds = 200, watchlist = list(train = dtrain, test = dtest), verbose = 0)


getwd()
setwd("C:/Users/Caitlin/Documents/GitHub/BIN381-Project/Four_Models_App/finalModels")

# Save the retrained model
saveRDS(xgb_mod, file = "xgb_sanitation_model.rds")
feature_names <- colnames(X_train)  # Assuming X_train is your one-hot encoded matrix
saveRDS(feature_names, file = "xgb_sanitation_feature_names.rds")

feature_names <- readRDS("xgb_sanitation_feature_names.rds")
print(feature_names)
