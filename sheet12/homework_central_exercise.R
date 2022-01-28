# Homework exercises

# section 00
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(ggrepel)
library(plotROC)
library(gridExtra)
library(caret)
library(rpart)
library(randomForest)
library(rpart.plot)

diabetes_dt <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/pima-indians-diabetes.csv")
diabetes_dt[, Outcome := as.factor(Outcome)]
# Store feature variables that we will need for later
feature_vars <- colnames(diabetes_dt[,-c("Outcome")])

full_formula <- as.formula(paste(c("Outcome ~ ",
                                   paste(feature_vars, collapse = " + ")),
                                 collapse = ""))
set.seed(13)
smp_size <- floor(0.7 * nrow(diabetes_dt))

train_ind <- sample(seq_len(nrow(diabetes_dt)), size = smp_size)

diabetes_dt[train_ind, dataset := "train"]
diabetes_dt[-train_ind, dataset := "test"]

train <- diabetes_dt[train_ind, ]
test <- diabetes_dt[-train_ind, ]

# ---

# section 03
# 1
diabetes_dt[, Outcome_str := ifelse(Outcome == 1, "yes", "no")]
# caret does not like factors, hence use string :(

full_formula_str <- as.formula(paste(c("Outcome_str ~ ",
                                       paste(feature_vars, collapse = " + ")),
                                     collapse = ""))

k <- 5    # number of folds
fitControl <- trainControl(method = "cv",
                           number = k, 
                           classProbs=TRUE,
                           summaryFunction = twoClassSummary)
lr_fit <- train(full_formula_str,
                data = train,
                method = "glm",
                family = "binomial",
                trControl = fitControl,
                metric = "ROC")
dt_results <- as.data.table(lr_fit$resample)
means_res <- dt_results[, mean(ROC)]

# 2
ordered_res <- dt_results[order(-ROC)]
# -> best fold: fold4

# 3
dt_results <- melt(dt_results, 
                   id.vars = 'Resample',
                   variable.name = 'metric')
boxplot_res <- ggplot(dt_results, aes(metric, value)) +
                geom_boxplot()

# 4 (optional)
# general trend: k=2 splits data half-half -> problematic if not much data available

for (k in c(2, 20, 100)) {
  # add code
}

# ---

# section 04
# 1 (optional)
best_auc <- 0
best_params <- c(0, 0)

possible_nodesizes <- c(10, 20, 30)
ntrees <- c(100, 150, 200)

get_rf_auc_4 <- function(formula, dt, train_ind, ntree, node_size){
  rf <- randomForest(formula,
                     data = dt[train_ind],
                     ntree = ntree,
                     nodesize = nodesize,
                     maxnodes = 7,
                     mtry = 5)
  dt[, prediction := (predict(rf, type="prob", newdata = dt)[, 2])]
  roc_forest <- ggplot(dt, aes(m=prediction, d=as.numeric(Outcome))) +
    geom_roc()
  res <- calc_auc(roc_forest)[3]
  return(res)
}

# grid search:
for (nodesize in possible_nodesizes){
  for (ntree in ntrees){
    current_auc <- get_rf_auc_4(formula = full_formula,
                                dt = diabetes_dt,
                                train_ind = train_ind,
                                ntree = ntree,
                                node_size = nodesize)
    if (current_auc > best_auc){
      best_auc <- current_auc
      best_params <- c(nodesize, ntree)
    }
  }
}

best_auc_rand <- 0
best_params_rand <- c(0, 0)
n_tries <- 10

# random search
for (n in n_tries){
  ntree <- sample(100:200, 1)
  nodesize <- sample(10:30, 1)
  
  current_auc <- get_rf_auc_4(formula = full_formula,
                              dt = diabetes_dt,
                              train_ind = train_ind,
                              ntree = ntree,
                              node_size = nodesize)
  if (current_auc > best_auc_rand){
    best_auc_rand <- current_auc
    best_params_rand <- c(nodesize, ntree)
  }
}

# ---

# section 05
# 1 (optional)
get_rf_auc_5 <- function(formula, dt, train_ind){
  rf <- randomForest(formula,
                     data = dt[train_ind],
                     ntree = 200,
                     nodesize = 20,
                     maxnodes = 7,
                     mtry = 5)
  dt[, prediction := (predict(rf, type="prob", newdata = dt)[, 2])]
  roc_forest <- ggplot(dt, aes(m=prediction, d=as.numeric(Outcome))) +
                  geom_roc()
  res <- calc_auc(roc_forest)[3]
  return(res)
}
total_auc <- get_rf_auc_5(formula = full_formula,
                        dt = diabetes_dt,
                        train_ind = train_ind)

importances <- c()
for (feat in feature_vars){
  curr_formula <- as.formula(paste(c("Outcome ~ ",
                                     paste(feature_vars[feature_vars != feat],
                                           collapse = " + ")),
                                   collapse = ""))
  current_auc <- get_rf_auc_5(formula = curr_formula,
                            dt = diabetes_dt,
                            train_ind = train_ind)
  importances <- c(importances, total_auc - current_auc)
}
importances_dt <- rbindlist(lapply(importances, as.data.table))
importances_dt[, feature_name := feature_vars]

importances_plot <- ggplot(importances_dt, aes(y = reorder(feature_name, -V1), x = V1)) +
                      geom_bar(stat = 'identity')
