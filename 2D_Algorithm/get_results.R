library(mclust)
library(psych)

get_results = function(result){
  
  # useful result measures
  # outputs tuckers congruence coefficient between true and estimated weights
  # and adjusted rand index between true and estimated partition
  
  partition = result$run$partition
  subject_count = length(partition)
  original_p = c(rep(1, subject_count/2), rep(2, subject_count/2))
  ari = adjustedRandIndex(partition, original_p)
  
  original_weights = result$data$weights
  weights = result$run$weights
  tuckers_vals = numeric()
  tuckers_vals_perm = numeric()
  cluster_indices = c(1,2)
  i = 1
  for (cluster_index in cluster_indices){
    for (block_index in c(1, 2)){
      tuckers_vals[i] = abs(as.numeric(factor.congruence(
        x = original_weights[[cluster_index]][[block_index]],
        y = weights[[cluster_index]][[block_index]])))
      cluster_index_perm = cluster_indices[!cluster_indices == cluster_index]
      tuckers_vals_perm[i] = abs(as.numeric(factor.congruence(
        x = original_weights[[cluster_index]][[block_index]],
        y = weights[[cluster_index_perm]][[block_index]])))
      i = i + 1
    }
  }
  avg_tuckers = mean(tuckers_vals)
  avg_tuckers_perm = mean(tuckers_vals_perm)
  
  return(list("Tuckers" = max(avg_tuckers, avg_tuckers_perm),
              "adjustedRandIndex" = ari))
}
