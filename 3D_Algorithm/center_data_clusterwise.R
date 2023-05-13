source("concatenate_cluster.R")

center_data_clusterwise = function( data , partition , cluster_count )
{
  # split data into clusters and center each cluster then reconstruct data
  # outputs data_centered - same structure as original but clusters are centered
  
  # data - list of two data blocks per subject
  # partition - vector describing partition of data
  # cluster_count - number of clusters
  
  data_centered = data
  subject_row_count = nrow(data[[1]][[1]]) # assuming modalities and subjects 
  # have same number of rows  
  for (cluster_index in 1:cluster_count)
  {
    cluster_subjects_index = which(partition == cluster_index) 
    cluster = concatenate_cluster(data, cluster_subjects_index)
    cluster_centered = lapply( cluster , function(X){
      scale(X, center = T, scale = T)} )

    # fill in data pertaining to cluster
    subject_rows_index = 1:subject_row_count
    for (subject_index in cluster_subjects_index)
    {
      data_centered[[subject_index]] = lapply(cluster_centered, function(X){
        X[subject_rows_index,]})
      subject_rows_index = subject_rows_index + subject_row_count
    }
  }
  
  return(data_centered)
}
