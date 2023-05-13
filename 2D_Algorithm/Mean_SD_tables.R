# load results table
# compute mean/sd of ari/tuckers for each factor level

mean_ari = mean(results_table$ari)
sd_ari = sd(results_table$ari)
mean_ari_by_subject_count = 
  aggregate(results_table$ari, list(results_table$subject_count), 
            FUN = function(x) c(mn = mean(x), sd = sd(x) ))
mean_ari_by_variable_count = 
  aggregate(results_table$ari, list(results_table$variable_count), 
            FUN = function(x) c(mn = mean(x), sd = sd(x) ))
mean_ari_by_max_weight = 
  aggregate(results_table$ari, list(results_table$max_weight), 
            FUN = function(x) c(mn = mean(x), sd = sd(x) ))
mean_ari_by_correlation = 
  aggregate(results_table$ari, list(results_table$correlation), 
            FUN = function(x) c(mn = mean(x), sd = sd(x) ))
mean_ari_by_noise = 
  aggregate(results_table$ari, list(results_table$noise), 
            FUN = function(x) c(mn = mean(x), sd = sd(x) ))

mean_tuckers = mean(results_table$tuckers)
sd_tuckers = sd(results_table$tuckers)
mean_tuckers_by_subject_count = 
  aggregate(results_table$tuckers, list(results_table$subject_count), 
            FUN = function(x) c(mn = mean(x), sd = sd(x) ))
mean_tuckers_by_variable_count = 
  aggregate(results_table$tuckers, list(results_table$variable_count), 
            FUN = function(x) c(mn = mean(x), sd = sd(x) ))
mean_tuckers_by_max_weight = 
  aggregate(results_table$tuckers, list(results_table$max_weight), 
            FUN = function(x) c(mn = mean(x), sd = sd(x) ))
mean_tuckers_by_correlation = 
  aggregate(results_table$tuckers, list(results_table$correlation), 
            FUN = function(x) c(mn = mean(x), sd = sd(x) ))
mean_tuckers_by_noise = 
  aggregate(results_table$tuckers, list(results_table$noise), 
            FUN = function(x) c(mn = mean(x), sd = sd(x) ))

print(mean_ari)
print(sd_ari)
print(mean_ari_by_subject_count)
print(mean_ari_by_variable_count)
print(mean_ari_by_max_weight)
print(mean_ari_by_correlation)
print(mean_ari_by_noise)

print(mean_tuckers)
print(sd_tuckers)
print(mean_tuckers_by_subject_count)
print(mean_tuckers_by_variable_count)
print(mean_tuckers_by_max_weight)
print(mean_tuckers_by_correlation)
print(mean_tuckers_by_noise)
