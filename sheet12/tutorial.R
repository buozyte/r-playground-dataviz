# Tutorial exercises

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

# ---

# section 01
# a
# False, as: by definition

# b
# False, as: biased/"wrong" model

# c
# False, as: overfitting always possible

# d
# False, as: possibly overfitting as error 0

# ---

# section 02
diabetes_dt <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/pima-indians-diabetes.csv")
diabetes_dt[, Outcome := as.factor(Outcome)]
# Store feature variables that we will need for later
feature_vars <- colnames(diabetes_dt[,-c("Outcome")])

# 1
full_formula <- as.formula(paste(c("Outcome ~ ",
                                   paste(feature_vars, collapse = " + ")),
                                 collapse = ""))
# We do not use this command full_formula <- Outcome~.
# Since we are adding some columns later and we do not want the formula to be altered
dt_classifier <- rpart(full_formula,
                       data = diabetes_dt,
                       control = rpart.control(minsplit = 3, cp = 0.001))
# rpart.plot(dt_classifier)
# summary(dt_classifier)

# 2
# predict(dt_classifier, type="prob")[, 2] <- only get second column, i.e. probabilities for Outcome 1
# remark: always use the probabilities for Outcome 1 in this lecture :)

diabetes_dt[, "prediction" := (predict(dt_classifier, type="prob")[, 2])]
roc <- ggplot(diabetes_dt, aes(m=prediction, d=as.numeric(Outcome))) +
        geom_roc() # + geom_abline()
# seems like a perfect model ... overfitting?

# test everything for only one split: one_split <- as.formula("Outcome ~ .") :)

# 3
set.seed(13)
smp_size <- floor(0.7 * nrow(diabetes_dt))

train_ind <- sample(seq_len(nrow(diabetes_dt)), size = smp_size)

train <- diabetes_dt[train_ind, ]
test <- diabetes_dt[-train_ind, ]

dt_classifier <- rpart(full_formula,
                       data = train,
                       control = rpart.control(minsplit = 3, cp = 0.001))
diabetes_dt[, prediction := (predict(dt_classifier, type="prob", newdata = diabetes_dt)[, 2])]

diabetes_dt[train_ind, dataset := "train"]
diabetes_dt[-train_ind, dataset := "test"]

roc_comparison <- ggplot(diabetes_dt, aes(m=prediction, d=as.numeric(Outcome), color=dataset)) +
                    geom_roc() # + geom_abline()
# overfitting detected!
calc_auc(roc_comparison)

# 4
rf_classifier <- randomForest(full_formula,
                              data = train,
                              ntree = 200,
                              nodesize = 20,
                              maxnodes = 7,
                              mtry = 5)
diabetes_dt[, prediction := (predict(rf_classifier, type="prob", newdata = diabetes_dt)[, 2])]
roc_forest <- ggplot(diabetes_dt, aes(m=prediction, d=as.numeric(Outcome), color=dataset)) +
                geom_roc()
calc_auc(roc_forest)

# 5 (optional)
# see official solution :)
