library(Matrix)
library(CCA)
source("concatenate_cluster.R")


compute_single_CCA = function( data, cluster_subjects_index , component_count )
{
  # compute CCA parameters for a single clusters using CCA library
  
  # data - list of two data blocks per subject
  # cluster_subjects_index - indexes of subjects in cluster being analysed
  # component_count - number of components in CCA
  
  # create cluster
  cluster = concatenate_cluster(data, cluster_subjects_index)
  
  cc_results = cc(cluster[[1]], cluster[[2]])
  
  weights = list(cc_results$xcoef[, 1:component_count], 
                 cc_results$ycoef[, 1:component_count])
  can_variates = list(cc_results$scores$xscores[, 1:component_count],
                      cc_results$scores$yscores[, 1:component_count])
  can_correlations = cc_results$cor[1:component_count]
  
  # subject loss
  if (component_count == 1){
    split_subject_loss = (can_variates[[1]] - can_variates[[2]])**2 
  }
  else{
    split_subject_loss = rowSums((can_variates[[1]] - can_variates[[2]])**2 )  
  }
  
  subject_row_count = nrow(data[[1]][[1]])
  subject_loss = colSums(matrix(split_subject_loss, nrow = subject_row_count))
  cluster_loss = sum(subject_loss)
  
  return( list(
    'weights' = weights, 
    'canonical correlations' = can_correlations,
    'canonical variates (reduced)' = can_variates,
    'loss' = list('cluster loss' = cluster_loss,
                  'loss per subject' = subject_loss )
    ))
}
