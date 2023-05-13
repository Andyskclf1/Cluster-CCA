concatenate_cluster = function(data, cluster_subjects_index){
  
  # concatenate subjects belonging to same cluster
  # outputs list of two data blocks
  
  # data - list of two data blocks per subject
  # cluster_subjects_index - indexes of subjects in cluster being analysed
  
  # iterate over blocks and concatenate matrices using do.call and cbind 
  block_count = length(data[[1]])
  cluster_subjects_count = length(cluster_subjects_index)
  cluster = list()
  
  for (block_index in 1:block_count){
    block_unconc = list()
    block_unconc_index = 1
    for (cluster_subject_index in cluster_subjects_index){
      block_unconc[[block_unconc_index]] = 
        data[[cluster_subject_index]][[block_index]]
      block_unconc_index = block_unconc_index + 1
    }
    cluster[[block_index]] = do.call(rbind, block_unconc)
  }
  
  return(cluster)

}
