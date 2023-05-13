source( "compute_CCA.R" )
source( "initialize_p.R" )
source( "update_p.R" )
source( "center_data_clusterwise.R" )


cluster_CCA = function( initial_partition = NULL , cluster_count ,
                         component_count , data,
                         center = T , tolerance = 0.0001 , verbose = T ,
                         ALS_iteration_count = 100 , show_info = F )
{
  # Cluster-CCA for three-way data
  # Outputs list of final partition, weights, correlation matrices and losses
  
  # initial_partition - whether or not a starting partition exists (for ILS)
  # cluster_count - number of clusters
  # component_count - number of components
  # data - list of two data blocks per subject
  # tolerance - convergence criterion parameter
  # center - logical, T if you want data to be centered
  # ALS_iteration_count - number of iterations for ALS algorithm
  # show_info - logical value, T for extra information when running algorithm

  subject_count = length(data) # total number of subjects in the data
  variable_count = ncol(data[[1]][[1]]) # number of variables per block
  global_loss = numeric()
  approx_global_loss = numeric()
  sum_subject_loss = numeric()
  first_can_cor_iterations = list()
  
  #### argument checks #### 
  
  # minimum cluster size
  # number of cases should be larger than variables since we do not use 
  # regularized CCA
  if( (subject_count/cluster_count) >= (variable_count+5) )
  {
    min_cluster_size = variable_count+5
  }
  else{
    stop("too many variables compared to subjects")
  }
  
  # constrain component_count
  if (component_count >= min(min_cluster_size, variable_count))
  {
    stop("number of components too large, solution non-unique")
  }

  #### STEP 0a ####
  # define starting partition
  if( is.null(initial_partition) )
  {  
    partition = initialize_p( cluster_count , subject_count , min_cluster_size )
  }
  else
  {
    partition = initial_partition
  }
  
  #### center ####
  if (center == T)
  {
    data = center_data_clusterwise( data , partition , cluster_count )
  }
  
  loss_overview = NULL
  z = 0 
  
  #### STEP 0b ####
  # compute cluster specific CCA parameters
  if (show_info == T)
  {
    print('computing CCA paramaters')     
  }
  
  GCCA_pars = compute_CCA( partition , cluster_count ,
                                component_count , subject_count , data )
  weights = GCCA_pars$weights
  global_loss[z+1] = GCCA_pars$loss$`global loss`
  loss_overview = c( loss_overview , global_loss[z+1] )
  current_cluster_loss = GCCA_pars$loss$`cluster loss`
  subject_loss = GCCA_pars$loss$`loss per subject (all)`
  for (cluster_index in 1:cluster_count){
    first_can_cor_iterations[[cluster_index]] =
      GCCA_pars$`canonical correlations`[[cluster_index]][1]
  }
  
  if (show_info == T)
  {
    cat('global loss: ', global_loss[z+1], fill = T)
  }
  
  repeat
  { 
    # STEP 1+0b until convergence (STEP 2)
    #### STEP 1 #### - update partition
    z = z + 1
    if (show_info == T)
    {
      cat('iteration', z, fill = T)
      print('updating clusters')     
    }
    
    update = update_p( partition , current_cluster_loss , 
                           cluster_count , subject_count ,
                           component_count , data , min_cluster_size ,
                           show_info , center , subject_loss )
    partition = update$partition
    approx_global_loss[z] = update$`approx global loss`
    loss_overview = c( loss_overview , approx_global_loss[z] )
    if (center == T)
    {
      data = center_data_clusterwise( data , partition , cluster_count )
    }
    
    #### STEP 0b #### - compute new clusters and loss
    if (show_info == T)
    {
      print('computing CCA paramaters')     
    }
    
    updated_pars = compute_CCA( partition , cluster_count ,
                                     component_count , subject_count , data )
    weights = updated_pars$weights
    global_loss[z+1] = updated_pars$loss$`global loss`
    loss_overview = c( loss_overview , updated_pars$loss$`global loss` )
    current_cluster_loss = updated_pars$loss$`cluster loss`
    subject_loss = updated_pars$loss$`loss per subject (all)`
    for (cluster_index in 1:cluster_count){
      first_can_cor_iterations[[cluster_index]][z+1] =
        updated_pars$`canonical correlations`[[cluster_index]][1]
    }
    
    if (show_info == T)
    {
      cat( 'global loss: ' , global_loss[z+1] , fill = T )
    }
    
    # check convergence, if true, save final canonical correlations,
    # global loss and subject losses
    perc_gain = (global_loss[z] - global_loss[z+1]) / global_loss[z]
    check1 = ( perc_gain < tolerance )
    check2 = (z == ALS_iteration_count)
    if( check1 | check2 )
    {
      can_correlations = updated_pars$`canonical correlations`
      final_global_loss = global_loss[z+1]
      subjects_loss = updated_pars$loss$`loss per subject (all)`
      break()
    }
  }
  
  # return cluster GCCA final parameters and other useful information
  return(list(
    'partition' = partition,
    'weights' = weights,
    'canonical correlations' = list(
      'final canonical correlations' = can_correlations,
      'first canonical correlation iterations' = first_can_cor_iterations),
    'loss' = list('final global loss' = final_global_loss, 
                  'global loss iterations' = global_loss,
                  'subjects loss' = subjects_loss,
                  'clusters loss' = current_cluster_loss,
                  'loss overview'=loss_overview )
  ))
}
