

#looking at the structure of the saved models
getwd()
list.files()
setwd("C:/Users/Caitlin/Documents/GitHub/BIN381-Project/Four_Models_App/finalmodels")
m <- readRDS("rf_nutrition_model.rds")
str(m, max.level = 2)

fNames<- readRDS("xgb_sanitation_feature_names.rds")
str(m, max.level = 2)

m<-readRDS("xgb_sanitation_model.rds")
str(m, max.level = 2)


m<-readRDS("rf_model_GroupD_Social.rds")
str(m, max.level = 2)
