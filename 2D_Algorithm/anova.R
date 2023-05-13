# load results table
# compute ANOVA for ari and tuckers

library(effectsize)
library(car)

# treat data parameters as factors
results_table$subject_count = as.factor(results_table$subject_count)
results_table$variable_count = as.factor(results_table$variable_count)
results_table$max_weight = as.factor(results_table$max_weight)
results_table$correlation = as.factor(results_table$correlation)
results_table$noise = as.factor(results_table$noise)

# cluster recovery (ari)
anova_ari <- aov(
  ari ~ subject_count * variable_count * max_weight * 
    correlation * noise, 
  data = results_table,
  contrasts = list(
    subject_count = "contr.sum",
    variable_count = "contr.sum",
    max_weight = "contr.sum",
    correlation = "contr.sum",
    noise = "contr.sum"
  )
)
summary(anova_ari)
eta_squared(car::Anova(anova_ari, type = 3), generalized = T)

# weight recovery (tuckers)
anova_tuckers <- aov(
  tuckers ~ subject_count * variable_count * max_weight * 
    correlation * noise, 
  data = results_table,
  contrasts = list(
    subject_count = "contr.sum",
    variable_count = "contr.sum",
    max_weight = "contr.sum",
    correlation = "contr.sum",
    noise = "contr.sum"
  )
)
summary(anova_tuckers)
eta_squared(car::Anova(anova_tuckers, type = 3), generalized = T)
