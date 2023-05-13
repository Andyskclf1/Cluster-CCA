source("check_cluster2.R")
source("center_data_clusterwise.R")
source("compute_single_CCA.R")

update_p = function( partition , current_cluster_loss , cluster_count ,
                          subject_count , component_count , data ,
                         min_cluster_size , show_info = show_info , center ,
                         subject_loss )
{
  # update partition 
  # outputs updated partition vector and estimate global loss
  
  # partition - current partition vector
  # current_cluster_loss - current vector of loss per cluster
  # cluster_count - number of clusters
  # subject_count - number of subjects
  # component_count - number of components
  # data - list of data blocks
  # center - logical, T if you want data to be centered
  # show_info - logical value, T for extra information when running algorithm
  # min_cluster_size - minimum number of subjects in a cluster
  # subject_loss - vector of loss per subject
  
  # begin looking at each subject for optimal cluster update
  # vector of minimum approx losses for each subject to be used later
  min_subject_losses = numeric( subject_count )

  partition_update = partition
  subject_order = sample(1:subject_count)
  for( subject_index in subject_order )
  {
    # find optimal solution for given subject
    temp_sol = check_cluster2( data , partition_update , subject_index ,
                                current_cluster_loss , component_count ,
                                subject_loss )
    # update cluster for given subject 
    partition_update[subject_index] = temp_sol$opt_clust
    
    # update vector of loss per cluster
    current_cluster_loss = temp_sol$cluster_loss
    
    # update vector of loss per subject
    subject_loss = temp_sol$subject_loss
    rm(temp_sol)
  }
  
  min_subject_losses = subject_loss
  
  if (center == T)
  {
    data = center_data_clusterwise( data , partition_update , cluster_count )
  }
  
  
  ##############################################################################
  ##############################################################################
  ##############################################################################
  
  # ensure no clusters smaller than min_cluster_size 
  # vector of cluster sizes, not including empty clusters
  cluster_sizes = as.vector( table(partition_update) )

  while( min(cluster_sizes) < min_cluster_size || 
        # second condition checks for empty clusters
        length(cluster_sizes) < cluster_count)
  {
    # for each cluster check whether it fits requirements
    for (cluster_index in 1:cluster_count)
    {
      if (sum(cluster_index == partition_update) < min_cluster_size)
      {
        
        # if cluster not ok, add subject with largest loss that satisfies 
        # constraint vector of subject loss rankings in index form (descending)
        worst_subject_ranking = sort(min_subject_losses, decreasing = TRUE,
                                    index.return = T)$ix
      
        # search for worst fitting subject that satisfies constraint
        for(worst_subject_index in worst_subject_ranking)
        {
          # update partition only if cluster losing subject remains correct
          worst_cluster = partition_update[worst_subject_index] 
          worst_cluster_updated_size = sum(worst_cluster == partition_update) - 1
        
        
          if(worst_cluster_updated_size >= min_cluster_size)
          {
            partition_update[worst_subject_index] = cluster_index
            if (center == T)
            {
              data = center_data_clusterwise( data , partition_update ,
                                              cluster_count )
            }
            break()
          }
        }
        
        # update min_subject_losses for cluster that has lost subject and cluster
        # that has gained subject if possible
        for (affected_cluster_index in c(worst_cluster, cluster_index))
        {
          # first check that cluster is eligible for parameter update
          if (sum(affected_cluster_index == partition_update) >= min_cluster_size)
          {
            # indices of rows to select
            cluster_subjects_index = which(partition_update == affected_cluster_index)
            
            # recompute parameters
            pars = compute_single_CCA( data, cluster_subjects_index , component_count )
            
            # retrieve subject-specific losses for cluster
            min_subject_losses[cluster_subjects_index] = pars$loss$`loss per subject` 
          }
        }
      }
    }
    
    # update vector of cluster sizes
    cluster_sizes = as.vector( table(partition_update) )
  }
  
  return( list( 'partition' = partition_update , 
                'approx global loss' = sum(min_subject_losses) ) )
}