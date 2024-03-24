###############################################################################
# MET CS699
# Final Project
# Brandon Bartol & Chuong Nguyen
###############################################################################

library(modeest)
library(dplyr)
library(caret)
library(Boruta)

dir <- file.path("C:", "Users", "Admin", "iCloudDrive",
                 "CS699 - Data Mining", "FinalProject", "gitHub")
setwd(dir)

# setwd("C:\\Users\\brand\\OneDrive\\Documents\\699\\TermProject\\Data")

Project_Data <- read.csv("project_dataset_5K.csv",header = TRUE)
Project_Data

Project_Data_Cleansed <- Project_Data
sapply(Project_Data_Cleansed, class)

###############################################################################
# Part I: Data preprocessing
###############################################################################

#Count missing data in each column
sapply(Project_Data_Cleansed, function(x) sum(is.na(x)))
#remove columns with more than 90% of the data present
Project_Data_Cleansed2 <- Project_Data_Cleansed %>%
  select_if(function(x) sum(is.na(x)) < 1000) # INCREASED from 500 to keep 2 columns with 508 na values


sapply(Project_Data_Cleansed2, function(x) sum(is.na(x)))


#Add data to columns with null values:
#if column contains <= 5 unique values, then the mode is applied
#else the median is applied
#this is stratified for the Class column
grouped <- split(Project_Data_Cleansed2, Project_Data_Cleansed2$Class)

for (col_name in colnames(Project_Data_Cleansed2)) {
  unique_values <- unique(Project_Data_Cleansed2[[col_name]])
  
  if (length(unique_values) <= 5) {
    # Compute mode value for each group
    mode_value <- sapply(grouped, function(group) {
      mfv(group[[col_name]][!is.na(group[[col_name]])])
    })
    Project_Data_Cleansed2[[col_name]] <- ifelse(is.na(Project_Data_Cleansed2[[col_name]]), mode_value[Project_Data_Cleansed2$Class], Project_Data_Cleansed2[[col_name]])
  } else {
    # Compute median value for each group
    non_missing_values <- Project_Data_Cleansed2[[col_name]][!is.na(Project_Data_Cleansed2[[col_name]])]
    median_value <- median(non_missing_values)
    Project_Data_Cleansed2[[col_name]] <- ifelse(is.na(Project_Data_Cleansed2[[col_name]]), median_value, Project_Data_Cleansed2[[col_name]])
  }
}


#find highly correlated columns to remove duplicate influence (corr > .7)
corr <- cor(Project_Data_Cleansed2[,-ncol(Project_Data_Cleansed2)])
findCorrelation(corr, cutoff = 0.7, names = TRUE)
CorrColumnRemoved <- findCorrelation(corr, cutoff = 0.7, names = FALSE)
Project_Data_Cleansed2 <- Project_Data_Cleansed2[, -CorrColumnRemoved]

#removing columns with near zero variance, changed frequency to 20 to match class notes
nearZeroVar(Project_Data_Cleansed2, freqCut = 20, saveMetrics =  TRUE)
near_zero_vars <- nearZeroVar(Project_Data_Cleansed2, freqCut = 20, saveMetrics = FALSE)
Project_Data_Cleansed2 <- Project_Data_Cleansed2[, -near_zero_vars]

sapply(Project_Data_Cleansed2, class)


###############################################################################
# Part II: Apply feature selection and unbalanced-data treatment models
###############################################################################

###########################
# Feature selection 1
###########################

#Use boruta to identify unimportant attributes
Project_Data_Cleansed3 <- Project_Data_Cleansed2
Project_Data_Cleansed3$Class <- factor(Project_Data_Cleansed2$Class, levels = c("N", "Y"))
borutaResult <- Boruta(Class ~ ., data = Project_Data_Cleansed3)
borutaResult$finalDecision
#remove rejected columns
BorutaRejectedColumns <- names(borutaResult$finalDecision[borutaResult$finalDecision == "Rejected"])
Project_Data_Cleansed3 <- Project_Data_Cleansed3[,!(names(Project_Data_Cleansed3) %in% BorutaRejectedColumns)]


#Used Code provided in Class to do PCA
df <- Project_Data_Cleansed3[, -(1:9)]
sapply(df, class)

# split dataset
set.seed(123)
split <- initial_split(df, prop = 0.66, strata = Class)
training <- training(split)
test <- testing(split)


###########################
# Feature selection 2
###########################

# build Weka’s J48 decision tree model using all attributes
library(RWeka)
J48.model <- J48(Class ~ . , data=training)
# test the model on the test dataset
pred <- predict(J48.model, newdata = test, type = "class")
performance_measures  <- confusionMatrix(data=pred,
                                         reference = test$Class)
performance_measures

# apply PCA on the training dataset
pc <- prcomp(training[, -ncol(training)] %>% select_if(~ all(is.numeric(.))), center = TRUE, scale = TRUE) # exclude class attribute
summary(pc)
#PCA finds 32 principal components, we can preserve 90% of the total variability.


###########################
# Feature selection 3
###########################




###############################################
# Data scaling / undersampling + oversampling 1
###############################################





###############################################
# Data scaling / undersampling + oversampling 2
###############################################




##############################################
# Data visualizations
##############################################





###############################################################################
# Part III: Model Building
###############################################################################

##############################################
# Split training and testing sets
##############################################
# set.seed(123)



##############################################
# Model 1: 
##############################################



##############################################
# Model 2: 
##############################################



##############################################
# Model 3: 
##############################################



##############################################
# Model 4: 
##############################################



##############################################
# Model 5: 
##############################################



##############################################
# Model 6: 
##############################################



###############################################################################
# Part III: Model Evaluation & Interpretation
###############################################################################





