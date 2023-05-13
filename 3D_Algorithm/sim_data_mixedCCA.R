# Simulate data under Cluster-CCA with noise

library(mixedCCA)

add_noise=function(datablock,noise)
{
  # add Gaussian and scaled noise to data
  # output data block with added noise
  
  # datablock - data block
  # noise - level of noise between 0 and 1
  
  errorM=replicate(ncol(datablock),rnorm(nrow(datablock)))
  errorM=SSequal(errorM,datablock)
  errorlevel=noise/(1-noise)
  
  res=datablock + (errorM * sqrt(errorlevel))
  return(res)
}

SSequal=function(m1,m2)
{
  res=(m1/sqrt(sum(m1^2)) * sqrt(sum(m2^2)))
  return(res)
}

sim_data = function(subject_count = 100, variable_b_count = 10, correlation = 0.7,
                    cluster_count = 2, variable_a_count = 10, noise = 0.0001,
                    max_weight = 1){
  
  # simulate two-way Cluster-CCA data
  # output data and weights
  
  # subject_count - number of subjects
  # variable_count - number of variables per modality
  # correlation - correlation between canonical variates
  # cluster_count - number of clusters
  # noise - level of noise, between 0 and 1
  # max_weight - value of non-zero entry in weight vectors
  
  if (cluster_count*2 > variable_b_count){
    stop("variable_b count too small compared to cluster count")
  }
  
  weights = list()
  data = list()
  cluster_size_min = floor(subject_count/cluster_count)
  remainder = subject_count - cluster_size_min*cluster_count
  weight_position = 1
  clusters = list()
  start_subject_index = 1
  for (cluster_index in 1:cluster_count){
    
    if (cluster_index<=remainder){
      cluster_size = cluster_size_min + 1
    }
    else{
      cluster_size = cluster_size_min
    }
    
    CC = correlation
    t1 = numeric(variable_b_count)
    t1[weight_position] = max_weight
    weight_position = weight_position + 1
    t2 = numeric(variable_b_count)
    t2[weight_position] = max_weight
    weight_position = weight_position + 1
    p1 = length(t1)
    p2 = length(t2)
    finish_subject_index = start_subject_index + cluster_size - 1
    weights[[cluster_index]] = list(t1, t2)
    
    for (subject_index in start_subject_index:finish_subject_index){
      subject_data = GenerateData( n=variable_a_count, trueidx1=t1 , trueidx2=t2 ,
                                                Sigma1=diag(p1) , Sigma2=diag(p2) , 
                                                maxcancor=CC)
      data[[subject_index]] =  lapply(list(subject_data$X1, subject_data$X2), function(x){add_noise(x, noise)})
      
    }
    start_subject_index = finish_subject_index + 1
  }
  return(list('data' = data,
              'weights' = weights))
}
