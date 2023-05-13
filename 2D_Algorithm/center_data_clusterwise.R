center_data_clusterwise = function( data , partition , cluster_count )
{
  # split data into clusters and center each cluster then reconstruct data
  # outputs data_centered - same structure as original but clusters are centered
  
  # data - list containing two data blocks
  # partition - vector describing partition of data
  # cluster_count - number of clusters
  
  data_centered = data
  for (cluster_index in 1:cluster_count)
  {
    cluster_subjects_index = which(partition == cluster_index) 
    cluster_centered = lapply( data , function(X){
      scale(X[cluster_subjects_index,], center = T, scale = T)} )
    
    # fill in data rows pertaining to cluster
    block_count = length(data_centered)
    for (block_index in 1:block_count)
    {
      data_centered[[block_index]][cluster_subjects_index,] = 
        cluster_centered[[block_index]]
    }
  }
  
  return(data_centered)
}
