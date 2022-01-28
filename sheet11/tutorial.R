# Tutorial exercises

# section 00
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(ggrepel)
library(plotROC)
library(gridExtra)

# ---

# section 01
diabetes_dt <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/pima-indians-diabetes.csv")
diabetes_dt[, Outcome := as.factor(Outcome)]
# Store feature variables that we will need for later
feature_vars <- colnames(diabetes_dt[,-c("Outcome")])
# 1
# alternative absolute value: diabetes_dt[, table(Outcome)]
absolute_value <- diabetes_dt[, .N, by=Outcome]
relative_value <- diabetes_dt[, .N/nrow(diabetes_dt), by=Outcome]
# possible problem for logistic regression:
#   picking 0 is more likely as samples unbalanced

# 2
rel_ships <- grid.arrange(
  ggplot(diabetes_dt) +
    geom_boxplot(aes(Outcome, Glucose)), 
  ggplot(diabetes_dt) +
    geom_boxplot(aes(Outcome, BloodPressure)),
  ggplot(diabetes_dt) +
    geom_boxplot(aes(Outcome, Insulin)), ncol=2)

melted <- melt(diabetes_dt,
               id.vars=c("Outcome"),
               measure.vars=c("Glucose", "BloodPressure", "Insulin"))

rel_ships_alt <- ggplot(melted, aes(Outcome, value)) +
                    geom_boxplot() +
                    facet_wrap(~variable, scales="free_y", nrow=3)

# 3
logistic_fit <- glm(Outcome ~ Glucose, data=diabetes_dt, family="binomial")
# 0.03787 = beta1
# log odds (Outcome) = beta0 + beta1 * Glucose
# hence: delta log odds = beta1 * 1 = beta1
# odds = exp(beta1) approx 1.04

# interpretation:
#   glucose = 130 -> 1:1
#   glucose = 131 -> 1.04:1 (odds) -> 1.04/2.04 vs 1/2.04 (probability)
#   glucose = 132 -> 1.04*1.04:1
#   glucose = 140 -> (1.04^10):1

# hence: increase of glucose in 1mg/dl would change increase of odds of having
#         diabetes by 4%

# 4
diabetes_dt[, mu_hat := predict(logistic_fit, diabetes_dt)]
diabetes_dt[, probabilities := predict(logistic_fit, diabetes_dt, type="response")]
viz <- ggplot(diabetes_dt, aes(mu_hat, fill=Outcome)) +
        geom_histogram(position="dodge")
# would like to see: bimodal distribution, not much overlapping of the classes

viz_alt <- ggplot(diabetes_dt, aes(Glucose, probabilities)) +
            geom_point() +
            ylim(0, 1)

# 5
confusion_matrix_alt <- function(dt, score_column, labels_column, threshold){
  dt[, y_pred := ifelse(dt[, get(score_column)] > threshold, 1, 0)]
  dt[, table(dt[, get(labels_column)], dt[, y_pred])]
}

confusion_matrix <- function(dt, score_column, labels_column, threshold){
  dt[, table(dt[, get(labels_column)], get(score_column) > threshold)]
}

thresholds <- c(-1, 0, 1)
all_tables <- lapply(thresholds,
                     function(t){confusion_matrix(diabetes_dt, "mu_hat", "Outcome", t)})
# False Positives: cell TRUE (predicted class: 1) and 0 (actual class: 0)

# 6
# TODO :(
tpr_fpr <- function(dt, score_column, labels_column, threshold){
  confusion <- confusion_matrix(dt, score_column, labels_column, threshold)
  tpr <- confusion["1", "TRUE"] / (confusion["1", "TRUE"] + confusion["1", "FALSE"])
  fpr <- confusion["0", "FALSE"] / (confusion["0", "FALSE"] + confusion["0", "TRUE"])
  return(data.table(tpr=tpr, fpr=fpr, t=threshold))
}

all_rates <- rbindlist(lapply(thresholds,
                              function(t){tpr_fpr(diabetes_dt, "mu_hat", "Outcome", t)}))
rates <- ggplot(all_rates, aes(fpr, tpr, label=t)) +
          geom_point() +
          geom_text_repel() +
          xlim(0, 1) + ylim(0, 1)