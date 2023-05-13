source("cluster_CCA.R")


cluster_CCA_multi= function(multi_iteration_count = 100,
                            ALS_iteration_count  = 100, cluster_count, 
                            component_count, data, verbose = F,
                            center = F, tolerance = 0.0001, 
                            initial_partition = NULL){
  
  # random start multi-start procedure
  
  # multi_iteration_count - number of random starts
  # initial_partition - whether or not a starting partition exists
  # cluster_count - number of clusters
  # component_count - number of components
  # data - list of two data blocks per subject
  # tolerance - convergence criterion parameter
  # center - logical, T if you want data to be centered
  # ALS_iteration_count - number of iteration used in ALS algorithm
  # show_info - logical value, T for extra information when running algorithm
  
  # run algorithm for first time
  cluster_CCA_best = cluster_CCA(initial_partition = initial_partition, 
                                   cluster_count,component_count, data, 
                                   center, tolerance, verbose,
                                   ALS_iteration_count)
  loss_best = cluster_CCA_best$loss$`final global loss`
  
  for (iteration in 1:multi_iteration_count){
  
    cluster_CCA_new = cluster_CCA(initial_partition, cluster_count, component_count, 
                                    data, center, 
                                    tolerance, verbose, ALS_iteration_count)
    loss_new = cluster_CCA_new$loss$`final global loss`
    
    # if lower loss than loss_best, update best
    if (loss_new < loss_best){
      loss_best = loss_new
      cluster_CCA_best = cluster_CCA_new
    }
  }
  
  return(cluster_CCA_best)
}
