library(data.table)
library(tidyverse)
library(caret)
library(ROCR)
library(ggplot2)
library(dplyr)
library(car)
library(caTools)
library(rpart)
library(rpart.plot)  
library(ROSE)
library(openxlsx)
setwd("/Users/quyichen/Desktop/Dream_Started/BC2406/Project Requirements and Guidelines")

df <- fread("heart_disease.csv")


colnames(df)
summary(df)
# Pre-EDA
# ==============================================================================
num_cols <- names(df)[sapply(df, is.numeric)]
cat_cols <- names(df)[!sapply(df, is.numeric)]

  # Missing values table (all columns)
missing <- colSums(is.na(df))
missing_pct <- round(missing / nrow(df) * 100, 2)
missing_tbl <- data.frame(Missing_Count = missing, Missing_Percentage = missing_pct, row.names = NULL)
missing_tbl <- cbind(Column = names(missing), missing_tbl)
print(missing_tbl)

  # Numerical columns: calculate mean and standard deviation
if (length(num_cols) > 0) {
  num_stats <- data.frame(
    Column = num_cols,
    Mean = sapply(num_cols, function(col) mean(df[[col]], na.rm = TRUE)),
    Standard_Deviation = sapply(num_cols, function(col) sd(df[[col]], na.rm = TRUE))
  )
  num_stats$Mean <- round(num_stats$Mean, 2)
  num_stats$Standard_Deviation <- round(num_stats$Standard_Deviation, 2)
}

print(num_stats)

  # Numerical columns: one histogram per column
for (col in num_cols) {
  data_vec <- df[[col]]
  data_vec <- data_vec[!is.na(data_vec)]
  
  # Create histogram using ggplot2
  p <- ggplot(data.frame(x = data_vec), aes(x = x)) +
    geom_histogram(bins = 30, fill = "gray70", color = "black") +
    labs(title = paste("Histogram of", col),
         x = col,
         y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}
print(p)

  # Categorical columns: one pie chart per column + frequency table

for (col in cat_cols) {
  # Frequency including NA as a category
  freq <- table(df[[col]], useNA = "always")
  freq_df <- data.frame(
    Category = names(freq),
    Count = as.numeric(freq),
    Percentage = round(as.numeric(freq) / nrow(df) * 100, 2)
  )
  
  # Replace NA with "NaN" for readability
  freq_df$Category[is.na(freq_df$Category)] <- "NaN"
  
  # Pie chart using ggplot2
  p <- ggplot(freq_df, aes(x = "", y = Count, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = pi / 2) +
    labs(title = paste("Distribution of", col)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              position = position_stack(vjust = 0.5),
              size = 3,
              color = "black") +
    guides(fill = guide_legend(title = col))
}
print(p)

# Data cleaning
# ==============================================================================
# 1. Replace spaces as "_" in column names
setnames(df, gsub(" ", "_", names(df)))

# 2. Drop na first
df[df == ""] <- NA
colSums(is.na(df))

df <- df[complete.cases(df), ]

colSums(is.na(df))

# 3. change the targeted variable into numeric
colnames(df)[colnames(df) == "Heart_Disease_Status"] <- "target"
df$target <- ifelse(df$target == "Yes", 1, 0) 
df$target <- as.factor(df$target)

# 4. Convert character strings to factors (Preparation for Rose)
char_cols <- names(df)[sapply(df, is.character)]
df[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]

# 5. Create balanced data set
set.seed(42)
df_bal <- ROSE(target ~ ., data = df, seed = 42)$data
table(df_bal$target)

# 6. Assign value into data clean
df_clean <- df_bal

# Some Visualization
# ==============================================================================
# 1. Check the variable importance to do the feature engineering
set.seed(123)
model_caret <- train(target ~ ., data = df_clean, method = "glm", family = "binomial",
                     trControl = trainControl(method = "cv", number = 5))

var_imp <- varImp(model_caret, scale = TRUE) 


plot(var_imp, main = "Variable Importance for All Features")
var_imp_df <- as.data.frame(var_imp$importance)
var_imp_df$Variable <- rownames(var_imp_df)
print(var_imp_df)

ggplot(var_imp_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance for All Features", x = "Variables", y = "Importance") +
  theme_minimal()

# Findings: 
# 1. Some variables are barely significant, so they can be removed outright.
# 2. Certain continuous variables such as BMI, blood sugar, and blood pressure exhibit significant centralization, which can be mitigated by binning to reduce noise.
# 3. Gender, alcohol intake, and stress are variables of extremely high significance, which should be retained.

# 2. Heart disease distribution(balanced)
table_hd <- table(df_clean$target)
table_hd <- as.data.frame(table_hd)
colnames(table_hd) <- c("Heart_Disease_Status", "Count")
print(table_hd)

ggplot(df_clean, aes(x = target, fill = target)) +
  geom_bar(width = 0.6) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
  labs(
    title = "Balanced Distribution of Heart Disease Target Variable",
    subtitle = "After ROSE Oversampling",
    x = "Target Class",
    y = "Count"
  ) +
  theme_minimal()

# Feature engineering
# ==============================================================================
str(df_clean)

# 1. Delete columns with extremely low importance
df_clean <- df_clean %>%
  select(-Sleep_Hours, -Low_HDL_Cholesterol, -High_LDL_Cholesterol,
         -Diabetes, -Cholesterol_Level,-Age)

# 2. Blood sugar bining (Since fasting blood sugar has higher importance, similar class sugar consumption can be deleted)
df_clean <- df_clean %>%
  mutate(BS_C = cut(Fasting_Blood_Sugar,
                    breaks = c(-Inf, 70, 100, Inf),
                    labels = c("Low", "Medium", "High"),
                    include.lowest = TRUE)) %>%
  select(-Fasting_Blood_Sugar, -Sugar_Consumption)

# 3. BMI binning
df_clean <- df_clean %>%
  mutate(
    BMI_C = cut(
      BMI,
      breaks = c(-Inf, 18.5, 25, 30, Inf),          
      labels = c("Underweight", "Normal", "Overweight", "Obese"),
      right = FALSE                         
    )
  ) %>%
  select(-BMI)   

# 4. Blood pressure bining (Since blood pressure has a higher importance, similar class high blood pressure can be deleted)
df_clean <- df_clean %>%
  mutate(
    BP_C = cut(
      Blood_Pressure,
      breaks = c(-Inf, 120, 140, Inf),              
      labels = c("Normal", "Elevated", "High"),
      right = FALSE
    )
  )


df_clean <- df_clean %>% select(-Blood_Pressure)
df_clean <- df_clean %>% select(-High_Blood_Pressure)

# 5. Change all categorical value as factor
categorical_vars <- c("Gender", "Exercise_Habits", "Smoking", "Family_Heart_Disease", 
                      "Alcohol_Consumption", "Stress_Level")

df_clean <- as.data.frame(df_clean)
df_clean[categorical_vars] <- lapply(df_clean[categorical_vars], as.factor)

summary(df_clean)

# Post-EDA
# ==============================================================================
  # figure 1 ----
  # Target rate by BP category
df_clean %>%
  group_by(BP_C, target) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = BP_C, y = count, fill = target)) +
  geom_bar(stat = "identity", position = "fill") +
  ylab("Proportion") +
  ggtitle("Proportion of Heart Disease by Blood Pressure Category")

  # Target rate by BS category
df_clean %>%
  group_by(BS_C, target) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = BS_C, y = count, fill = target)) +
  geom_bar(stat = "identity", position = "fill") +
  ylab("Proportion") +
  ggtitle("Proportion of Heart Disease by Blood Sugar Category")

  # Target rate by BMI category
df_clean %>%
  group_by(BMI_C, target) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = BMI_C, y = count, fill = target)) +
  geom_bar(stat = "identity", position = "fill") +
  ylab("Proportion") +
  ggtitle("Proportion of Heart Disease by BMI Category")

  # figure 2 ----
  # Target rate by smoking
df_clean %>%
  group_by(Smoking, target) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = Smoking, y = count, fill = target)) +
  geom_bar(stat = "identity", position = "fill") +
  ylab("Proportion") +
  ggtitle("Proportion of Heart Disease by Smoking Status")

  # Target rate by exercise
df_clean %>%
  group_by(Exercise_Habits, target) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = Exercise_Habits, y = count, fill = target)) +
  geom_bar(stat = "identity", position = "fill") +
  ylab("Proportion") +
  ggtitle("Proportion of Heart Disease by Exercise Habits")


  # Target rate by alcohol
df_clean %>%
  group_by(Alcohol_Consumption, target) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = Alcohol_Consumption, y = count, fill = target)) +
  geom_bar(stat = "identity", position = "fill") +
  ylab("Proportion") +
  ggtitle("Proportion of Heart Disease by Alcohol Consumption")

  # Target rate by stress
df_clean %>%
  group_by(Stress_Level, target) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = Stress_Level, y = count, fill = target)) +
  geom_bar(stat = "identity", position = "fill") +
  ylab("Proportion") +
  ggtitle("Proportion of Heart Disease by Stress Level")

  # figure 3 & 4----
  # fig 3 
library(corrplot)

bio_data <- df_clean[, c("Triglyceride_Level", "CRP_Level", "Homocysteine_Level")]
cor_matrix <- cor(bio_data, use = "complete.obs")

corrplot(cor_matrix, method = "color", addCoef.col = "black",
         title = "Correlation Between Biochemical Markers",
         mar = c(0,0,2,0))

ggplot(df_clean, aes(x = Triglyceride_Level, y = CRP_Level, color = target)) +
  geom_point(alpha = 0.6) +
  labs(title = "CRP vs Triglyceride Level by Heart Disease Status",
       x = "Triglyceride Level",
       y = "CRP Level") +
  theme_minimal()

  # fig 4 
  # Boxplot for Triglycerides
ggplot(df_clean, aes(x = target, y = Triglyceride_Level, fill = target)) +
  geom_boxplot() +
  labs(title = "Triglyceride Level by Heart Disease Status",
       x = "Heart Disease",
       y = "Triglyceride Level")

  # Boxplot for CRP Level
ggplot(df_clean, aes(x = target, y = CRP_Level, fill = target)) +
  geom_boxplot() +
  labs(title = "CRP Level by Heart Disease Status",
       x = "Heart Disease",
       y = "CRP Level")

  # Boxplot for Homocysteine Level
ggplot(df_clean, aes(x = target, y = Homocysteine_Level, fill = target)) +
  geom_boxplot() +
  labs(title = "Homocysteine Level by Heart Disease Status",
       x = "Heart Disease",
       y = "Homocysteine Level")

  # figure 5 ----
df_clean <- df_clean %>%
  mutate(BP_BS_Combo = paste(BP_C, BS_C, sep = " & "))

combo_data <- df_clean %>%
  group_by(BP_BS_Combo, BMI_C) %>%
  summarise(
    heart_disease_rate = mean(target == "1"),  
    n = n(),
    .groups = 'drop'
  )

ggplot(combo_data, aes(x = BP_BS_Combo, y = heart_disease_rate, fill = BMI_C)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = scales::percent(heart_disease_rate, accuracy = 0.1)),
            position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  labs(
    title = "Heart Disease Proportion by Combined Blood Pressure & Blood Sugar",
    x = "Blood Pressure & Blood Sugar Combination",
    y = "Proportion with Heart Disease",
    fill = "BMI Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df_clean <- df_clean %>% select(-BP_BS_Combo)

# Statistical analysis
# ==============================================================================
# 1. Significance level
colSums(is.na(df_clean))
model <- glm(target ~ ., data = df_clean, family = binomial)

summary(model)

# 2. Check collinearity
vif_check <- car::vif(model)

print(vif_check)

# Modeling
# ==============================================================================
# 1. Separate train/text set
set.seed(42)

train <- sample.split(Y = df_clean$target, SplitRatio = 0.7)
trainset <- subset(df_clean, train == T)
testset <- subset(df_clean, train == F)

# 2. Build logistic regression
m1 <- glm(target ~ . , family = binomial, data = trainset)

summary(m1)

# 3. Prediction results (confusion matrix) on logistic regression 
threshold1 <- 0.5
prob.test <- predict(m1, newdata = testset, type = 'response')
m1.predict.test <- ifelse(prob.test > threshold1, 1, 0)
table1 <- table(Testset.Actual = testset$target, m1.predict.test, deparse.level = 2)
table1

round(mean(m1.predict.test == testset$target),4)

# 4. Build CART
set.seed(42)
m2 <- rpart(target ~ ., data = trainset, method = 'class',
            control = rpart.control(minsplit = 30,cp = 0)) 

rpart.plot(m2, nn= T, main = "Maximal Tree in df")

print(m2)

printcp(m2)
plotcp(m2, main = "Subtrees in df")

cp1<-0.002

m3 <- prune(m2, cp = cp1) # Our final model

print(m3)
printcp(m3)
rpart.plot(m3, nn= T, main = "Pruned Tree with cp = 0.002")

cart.predict <- predict(m3, newdata = testset, type = "class")

table2 <- table(Testset.Actual = testset$target, cart.predict, deparse.level = 2)
table2

round(mean(cart.predict == testset$target),4)
# Updating
# ==============================================================================
# 1. Preparation
library(randomForest)
library(xgboost)
library(pROC)
library(caret)
library(dplyr)
RF_train <- trainset
RF_test  <- testset

XGB_train <- trainset
XGB_test  <- testset

# 2. Random forest model
RF_train$target_factor <- as.factor(trainset$target)
RF_test$target_factor  <- as.factor(testset$target)

rf_model <- randomForest(
  target_factor ~ .,
  data = RF_train %>% select(-target),
  ntree = 500,
  mtry = floor(sqrt(ncol(RF_train) - 2)),
  importance = TRUE
)

print(rf_model)
varImpPlot(rf_model, main = "Random Forest - Feature Importance")

rf_pred_prob <- predict(rf_model, newdata = RF_test, type = "prob")[, 2]
rf_pred_class <- predict(rf_model, newdata = RF_test, type = "response")

cat("\n--- Random Forest Results ---\n")
roc_rf <- roc(
  response  = RF_test$target_factor,
  predictor = rf_pred_prob,
  levels    = c("0", "1"),
  direction = "<"
)
rf_acc <- mean(rf_pred_class == RF_test$target_factor)
auc_rf <- auc(roc_rf)

cat("Accuracy:", round(rf_acc, 4), "\n")
cat("AUC:", round(auc_rf, 4), "\n")

plot(roc_rf, main = paste("Random Forest ROC Curve (AUC =", round(auc_rf, 4), ")"))

  # importance plot
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)

importance_df <- importance_df[order(-importance_df$MeanDecreaseGini), ]  
top_features <- head(importance_df, 20)

ggplot(top_features, aes(x = reorder(Feature, MeanDecreaseGini), 
                         y = MeanDecreaseGini, fill = MeanDecreaseGini)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Feature Importances (Random Forest)",
    x = "Feature",
    y = "Mean Decrease in Gini"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  )


# 3. XGBoost model
train_matrix <- model.matrix(target ~ . -1, data = XGB_train)
test_matrix  <- model.matrix(target ~ . -1, data = XGB_test)

dtrain <- xgb.DMatrix(data = train_matrix, label = as.numeric(as.character(XGB_train$target)))
dtest  <- xgb.DMatrix(data = test_matrix,  label = as.numeric(as.character(XGB_test$target)))

xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.05,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_cv <- xgb.cv(
  params = xgb_params,
  data = dtrain,
  nrounds = 500,
  nfold = 5,
  early_stopping_rounds = 20,
  verbose = 1
)
best_nrounds <- xgb_cv$best_iteration

xgb_model <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = best_nrounds,
  verbose = 1
)

xgb_pred_prob <- predict(xgb_model, dtest)
xgb_pred_class <- ifelse(xgb_pred_prob > 0.5, 1, 0)

conf_mat <- table(Actual = XGB_test$target, Predicted = xgb_pred_class)
print(conf_mat)

xgb_acc <- mean(xgb_pred_class == XGB_test$target)
roc_xgb <- roc(as.numeric(as.character(XGB_test$target)), xgb_pred_prob)
auc_xgb <- auc(roc_xgb)

cat("Accuracy:", round(xgb_acc, 4), "\n")
cat("AUC:", round(auc_xgb, 4), "\n")

xgb_imp <- xgb.importance(model = xgb_model)
xgb.plot.importance(xgb_imp, top_n = 15, main = "XGBoost - Feature Importance")

plot(roc_xgb, main = paste("XGBoost ROC Curve (AUC =", round(auc_xgb, 4), ")"))


# 4. Comparision
cat("\n--- Model Comparison ---\n")
cat("Random Forest  Accuracy:", round(rf_acc, 4), " | AUC:", round(auc_rf, 4), "\n")
cat("XGBoost        Accuracy:", round(xgb_acc, 4), " | AUC:", round(auc_xgb, 4), "\n")

if (auc_rf > auc_xgb) {
  cat("✅ Random Forest performs better.\n")
} else {
  cat("✅ XGBoost performs better.\n")
}

