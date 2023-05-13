source("compute_single_CCA.R")

compute_CCA = function( partition , cluster_count ,
                         component_count, subject_count , data )
{
  # Compute CCA parameters for each cluster
  # Outputs list of parameters for each cluster, used for updating too
  
  # partition - current partition vector
  # cluster_count - number of clusters
  # component_count - number of components
  # data - list of data blocks
  # subject_count - number of subjects
  
  weights = list()
  can_correlations = list()
  first_can_correlations = list()
  cluster_loss = numeric( cluster_count ) # vector of losses per cluster
  #collects the loss for each subject (for all subjects)
  subject_loss = numeric( subject_count )
  
  # CCA per cluster
  for (cluster_index in 1:cluster_count)
  { 
    # construct list of reduced matrices per cluster
    # indices of rows to select
    cluster_subject_indices = which(partition == cluster_index) 

    # reduce matrices
    cluster = lapply( data , function(X){X[cluster_subject_indices,]} )
    pars = compute_single_CCA( cluster , component_count )
    
    weights[[cluster_index]] = pars$weights
    can_correlations[[cluster_index]] = pars$`canonical correlations`
    cluster_loss[cluster_index] = pars$loss$`cluster loss`
    subject_loss[cluster_subject_indices] = pars$loss$`loss per subject`
  } 
  
  # sum loss over each cluster
  global_loss = sum(cluster_loss)
  
  return(
    list(
    'weights' = weights, 
    'canonical correlations' = can_correlations,
    'loss' = list('global loss' = global_loss,
                  'cluster loss' = cluster_loss,
                  'loss per subject (all)' = subject_loss)
    ))
}
