library(Matrix)
library(CCA)


compute_single_CCA = function(cluster, component_count){
  
  # compute CCA parameters for a single clusters using CCA library
  
  # cluster - cluster data
  # component_count - number of components in CCA

  cc_results = cc(cluster[[1]], cluster[[2]])
  
  weights = list(cc_results$xcoef[, 1:component_count], 
                 cc_results$ycoef[, 1:component_count])
  can_variates = list(cc_results$scores$xscores[, 1:component_count],
                      cc_results$scores$yscores[, 1:component_count])
  can_correlations = cc_results$cor[1:component_count]
  
  if (component_count == 1){
    subject_loss = (can_variates[[1]] - can_variates[[2]])**2 
  }
  else{
    subject_loss = rowSums((can_variates[[1]] - can_variates[[2]])**2 )  
  }
  
  cluster_loss = sum(subject_loss)
  
  return( list(
    'weights' = weights, 
    'canonical correlations' = can_correlations,
    'canonical variates (reduced)' = can_variates,
    'loss' = list('cluster loss' = cluster_loss,
                  'loss per subject' = subject_loss )
  ))
}
