# Homework exercises

library(data.table)
library(tidyr)
library(ggplot2)
library(magrittr)
library(gridExtra)

# section 05
# 1
fatality_dt <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/belgium_infection_fatality_rate_june2020.csv")

fatality_melted <- melt(fatality_dt, id.vars='age_group', variable.name='sex')
plot_jitter <- ggplot(fatality_melted, aes(x=age_group, y=value, color=as.factor(sex))) +
                geom_jitter() +
                xlab(label="Age group") +
                ylab(label="Fatality rate")
plot_hist <- ggplot(fatality_melted, aes(x=age_group, y=value, fill=sex)) +
                geom_col(position=)

# Higher fatality rates are caused in both cases by higher age groups
# Women live longer, hence more older women than men and more older
#   women exist in general.
# The fatality rate might seem higher for women, as more older women
#   exist and have a higher fatality rate, while in reality this rate is
#   based on the higher numer of old women and their increased fatality
#   compared with younger women.
# -> Simpson's Paradox, first association is "women have higher rate",
#     but after grouping by third variable we can see, that "men have
#     higher rate"

# Idea: sex -> age -> fatality, sex -> fatality are the causes based on
#       available graph

# ---

# section 06
datavizitis_smoking_dt <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/datavizitis_smoking.csv")
# 1
plot_relation <- ggplot(datavizitis_smoking_dt[hospitalized=='Yes',],
                        aes(x=cigarettes_per_day,
                            y=datavizitis_severity)) +
                  geom_jitter() +
                  geom_smooth(method=lm)

# 2
plot_relation_all <- ggplot(datavizitis_smoking_dt,
                            aes(x=cigarettes_per_day,
                                y=datavizitis_severity)) +
                      geom_jitter() +
                      geom_smooth(method=lm)

# 3
plot_relation_col <- ggplot(datavizitis_smoking_dt,
                            aes(x=cigarettes_per_day,
                                y=datavizitis_severity)) +
                      geom_jitter(aes(color=hospitalized)) +
                      geom_smooth(method=lm) +
                      geom_smooth(method=lm, aes(color=hospitalized))
# Possible causal realtionship: severity -> hospitalized,
#                               cigarettes -> hospitalized
# Hence: common consequence

# 4
# different explanations, Berkson's paradox/common consequence.
# I think that non-smokers are more likely to be in the hospital due to
#   the severity of the disease, while smokers are also there due to
#   other reasons. Hence the severity of this disease causes the
#   severity of Covid-19.

# ---

# section 07
# 1
titanic_dt <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/titanic.csv")

# 2
summary(titanic_dt)
survival_rate <- titanic_dt[, mean(survived)] # == mean in summery

# 3
age_survival <- ggplot(titanic_dt, aes(y=age, x=as.factor(survived))) +
                  geom_violin() +
                  geom_boxplot()
# Age and survival does not seem very related

# 4
class_survival <- ggplot(titanic_dt, aes(x=pclass)) +
                    geom_histogram(bins=3) +
                    facet_wrap(~survived)
alt_class_survival <- ggplot(titanic_dt, aes(x=factor(pclass), fill=factor(survived))) +
                        geom_bar(position='fill')
# People from the third class were more like to die. The highest survival
#   rate is in the second class.

# 5
age_class <- ggplot(titanic_dt, aes(y=age, x=as.factor(pclass))) +
              geom_violin() +
              geom_boxplot()
# The lower the class, the younger the average age.
# The higher the class, the more old people are in the class.

# 6
all_together <- ggplot(titanic_dt, aes(y=age, x=as.factor(survived))) +
                  geom_boxplot() +
                  facet_wrap(~pclass)
all_plots <- grid.arrange(age_survival, age_class, class_survival, all_together, ncol=2, nrow=2)
# (4) shows that a lot of people from the third class died, while
#   (5) shows that most people from this class were rather young.
# People from the first class were generally older and more likely
#   to survive.
# This is (an common or) an indirect cause (based on interpretation).
# Possible relationships: age -> pclass -> survive, (age -> survived)

