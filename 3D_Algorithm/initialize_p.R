initialize_p = function(cluster_count, subject_count, cluster_size_min)
{
  # randomly partition subjects into non-empty clusters
  # outputs partition vector where nth element = cluster for subject n
  
  # cluster_count - number of clusters
  # subject_count - number of subjects
  # cluster_size_min - minimum number of subjects in a cluster

  repeat
  {
    partition = sample(1:cluster_count, size = subject_count, replace = T)
    
    cluster_sizes = as.vector(table(partition)) # vector of cluster sizes, 
    # not including empty clusters
    
    # ensure no clusters smaller than cluster_size_min
    if (min(cluster_sizes) >= cluster_size_min && 
        length(cluster_sizes) == cluster_count)
    {
      break()
    }  
  }
  
  return(partition)
}
