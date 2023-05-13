source("compute_single_CCA.R")

compute_CCA = function( partition , cluster_count ,
                         component_count, subject_count , data )
{
  # Compute CCA parameters for all clusters
  # Outputs list of parameters for each cluster, used for updating too
  
  # partition - current partition vector
  # cluster_count - number of clusters
  # component_count - number of components
  # data - list of two data blocks per subject
  # subject_count - number of subjects
  
  weights = list()
  can_correlations = list()
  cluster_loss = numeric( cluster_count ) # vector of losses per cluster
  subject_loss = numeric( subject_count )
  
  # do CCA per cluster
  for (cluster_index in 1:cluster_count)
  { 
    # subjects to select
    cluster_subjects_index = which(partition == cluster_index) 
    pars = compute_single_CCA( data, cluster_subjects_index , component_count )

    weights[[cluster_index]] = pars$weights
    can_correlations[[cluster_index]] = pars$`canonical correlations`
    cluster_loss[cluster_index] = pars$loss$`cluster loss`
    subject_loss[cluster_subjects_index] = pars$loss$`loss per subject`
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
