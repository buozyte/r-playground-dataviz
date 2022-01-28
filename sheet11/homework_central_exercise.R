# Homework exercises

# section 00
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(ggrepel)
library(plotROC)
library(gridExtra)

# ---

# section 02
diabetes_dt <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/pima-indians-diabetes.csv")
diabetes_dt[, Outcome := as.factor(Outcome)]
# Store feature variables that we will need for later
feature_vars <- colnames(diabetes_dt[,-c("Outcome")])
# 1
logistic_fit_g <- glm(Outcome ~ Glucose, data=diabetes_dt, family="binomial")
logistic_fit_bp <- glm(Outcome ~ BloodPressure, data=diabetes_dt, family="binomial")
logistic_fit_i <- glm(Outcome ~ Insulin, data=diabetes_dt, family="binomial")

# 2
diabetes_dt[, mu_hat_g := predict(logistic_fit_g, diabetes_dt)]
diabetes_dt[, probabilities_g := predict(logistic_fit_g, diabetes_dt, type="response")]
diabetes_dt[, mu_hat_bp := predict(logistic_fit_bp, diabetes_dt)]
diabetes_dt[, probabilities_bp := predict(logistic_fit_bp, diabetes_dt, type="response")]
diabetes_dt[, mu_hat_i := predict(logistic_fit_i, diabetes_dt)]
diabetes_dt[, probabilities_i := predict(logistic_fit_i, diabetes_dt, type="response")]

melted <- melt(diabetes_dt,
               measure.vars=c("mu_hat_g", "mu_hat_bp", "mu_hat_i"),
               variable.name="model",
               value.name="prediction")
hist <- ggplot(melted, aes(prediction, fill=Outcome)) +
          geom_histogram() +
          facet_wrap(~model)
# no bi-modality, classes overlapping :(

# 3
roc <- ggplot(melted, aes(m=prediction, d=as.numeric(Outcome), color=model)) +
        geom_roc()
# get area under curve for each model:
auc <- calc_auc(roc)

# bigger area -> better model
# hence: best model is first model (Glucose model)

# 4
concat <- paste0("Outcome ~ ", paste0(feature_vars, collapse="+"))
logistic_fit_full <- glm(concat, data=diabetes_dt, family="binomial")
sum_fm <- summary(logistic_fit_full)

diabetes_dt[, mu_hat_full := predict(logistic_fit_full, diabetes_dt)]
hist_full <- ggplot(diabetes_dt, aes(mu_hat_full, fill=Outcome)) +
              geom_histogram(position="dodge")

melted_full <- melt(diabetes_dt,
                    measure.vars=c("mu_hat_g", "mu_hat_bp", "mu_hat_i", "mu_hat_full"),
                    variable.name="model",
                    value.name="prediction")
roc_full <- ggplot(melted_full, aes(m=prediction, d=as.numeric(Outcome), color=model)) +
              geom_roc()
# get area under curve for each model:
auc_full <- calc_auc(roc_full)

# result: "full model" is the best model :)