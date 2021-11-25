# Tutorial exercises

# section 00
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)

# ---

# section 01
# 1
# As long as each category gets one color, it should be fine.
# Related categories should get similar colors.
# All in all: don't use too many colors, but enough.

# 2
# In general yes. Avoid colors if they don't add additionall information.
# Making a plot a bit prettier by adding one color should be fine though.

# 3
# No, divergent colours are often hard to distinguish.
# Hence using them for catergories can lead to confusion due to
#   too similar colors.

# ---

# section 02
# 1
# Reverse causality is more about the fact, that A and B are statistically
#   symmetric, but most of the time one is the cause and one is the effect.
# The problem here is that they are often confused due to the
#   statistical symmetry.

# 2
# Not always, see common cause.
# If A is for example a certain number of dice rolls, then B and C
#   could be the amount of certain sides. Then B does not cause C.

# 3
# No.
# Example: rooster sounds and sunrise are correlated, but the rooster
#           cannot cause the sunrise.

# 4
# No.
# There is a relation but it could also be quadratic or exponential.

# ---

# section 03
coffee_dt <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/coffee_sim.csv")
summary(coffee_dt)
# 1
a_coffee_disease <- ggplot(coffee_dt, aes(y=datavizitis_risk)) +
                    geom_boxplot() +
                    facet_wrap(~coffee_cups_per_day)

# Alternative:
# alt <- ggplot(coffee_dt, aes(x=coffee_cups_per_day, y=datavizitis_risk)) +
#                           geom_boxplot()

# 2
b_coffee_disease <- ggplot(coffee_dt, aes(x=coffee_cups_per_day, y=datavizitis_risk)) +
                      geom_boxplot() +
                      facet_wrap(~packs_cigarettes_per_day)
# depending on the packs of cigarettes per day, the deaths rise and the
#   effect of the amount of coffee stays nearly the same.

# Alternative 1:
# coffee_melted <- melt(coffee_dt, id.vars='datavizitis_risk')
# alt <- ggplot(coffee_melted, aes(x=value, y =datavizitis_risk, color=variable)) +
#         geom_point()

# Alternative 2:
# alt <- ggplot(coffee_dt, aes(x=coffee_cups_per_day, y=datavizitis_risk,
#                               fill=packs_cigarettes_per_day)) +
#         geom_boxplot()
# switch <- ggplot(coffee_dt, aes(x=packs_cigarettes_per_day, y=datavizitis_risk,
#                                 fill=coffee_cups_per_day)) +
#           geom_boxplot()

# Conclusion:
# Plot in 1 can lead to misinterpretation!

# Realtion to lecture: common cause
# ggplot(coffee_dt, aes(packs_cigarettes_per_day)) +
#   geom_histogram(stat="count") +
#   facet_wrap(~coffee_cups_per_day)
# ggplot(coffee_dt, aes(coffee_cups_per_day)) +
#   geom_histogram(stat="count") +
#   facet_wrap(~packs_cigarettes_per_day)

# ---

# section 04
# simulate data
dt <- data.table(pro_uptake = c(rnorm(3, 10100, 300),
                                rnorm(4, 12100, 300),
                                rnorm(3, 9850, 300),
                                rnorm(4, 11100, 300),
                                rnorm(4,8300, 300),
                                rnorm(3,10050, 300),
                                rnorm(3, 12000, 300),
                                rnorm(3, 10020, 300),
                                rnorm(3, 10080, 300),
                                rnorm(3, 10070, 300)
                                ),
                 mutants = c(rep("WT",3),
                             rep("T49A",4),
                             rep("K227N",3),
                             rep("A400V",4),
                             rep("L421P",4),
                             rep("I500T",3),
                             rep("N591D",3),
                             rep("A601T",3),
                             rep("E684D",3),
                             rep("G710R",3)
                             )
                 )
# 1
# Good:
# - simple design
# - not too many colors
# - clear labels/title
# - no chart junk

# Bad:
# - bad distinction of experiments or mutants (no differention by colour)
# - scale of y-axis (should be around data, not starting at zero for example)
# - x-axis not sorted
# - summary by mean and sd hides data which is at most 4 points per bar

# 2
# Suggestion:
# - plot single points (+boxplot) instead of bars
# - sort mutants by median
# - give color for above/below WT

dt[, median:=(sum(pro_uptake)/length(pro_uptake)), by=mutants]
median_wt <- dt[mutants=='WT', median][1]
dt[, where_wt:= as.factor((data.table::fcase(
                    dt$median < median_wt, 'below WT',
                    dt$median > median_wt, 'above WT',
                    dt$median == median_wt, 'WT')))
   ]

# alternative for ordering in aes:
# dt[, mutants:=factor(mutants, levels=unique(dt[order(median), mutants]))]

mutant_plot <- ggplot(dt, aes(x=reorder(mutants, pro_uptake, na.rm = TRUE),
                              y=pro_uptake,
                              color=where_wt)) +
                geom_boxplot() +
                geom_jitter(width=0.4) + 
                xlab("Mutant") +
                ylab("Uptake (cpm)")

