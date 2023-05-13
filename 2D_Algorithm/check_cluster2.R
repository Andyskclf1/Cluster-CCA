source("compute_single_CCA.R")

check_cluster2 <- function( data , partition , subjectNr ,
                               ClusterLossCurSol_init ,
                               component_count , subject_lossAll_init )
{
  # subject is reassigned to a new cluster based on the 
  # the loss of the subject in question only
  
  # data: a list with data matrices (equal nRow, possibly different nCol)
  # partition: current partition of (the rows of the) data
  # subjectNr: number of the subject to be considered for re-allocation
  # ClusterLossCurSol (1 x nClust): loss per cluster (of current solution 'partition')
  # component_count: number of components
  # subject_lossAll: loss per subject for current solution (in partition)
  
  #ClusterLossCurSol = ClusterLossCurSol_init
  
  nClust = length( ClusterLossCurSol_init )
  ClustCurObj = partition[subjectNr]
  LossPerObjVector = numeric( nClust )
  
  #loss for solution when subject is reassigned to each cluster
  LossSolReassigned = numeric( nClust )
  
  #loss for cluster in which subjectNr is removed
  tempx = which( partition==partition[subjectNr] )
  cluster_subjects_index_Minussubject = tempx[ tempx != subjectNr ] 
  cluster = lapply( data , function(X){X[cluster_subjects_index_Minussubject,]} )
  tempout = compute_single_CCA( cluster , component_count )
  LossClustMinussubject = tempout$loss$`cluster loss`
  subject_lossMinussubject = tempout$loss$`loss per subject`

    
  LossPerObjExtraCluster = NULL
  templossPerClust = NULL
  cluster_subjects_index = NULL
  for( clust in 1:nClust )
  {
    if( ClustCurObj == clust ) #when looking at the cluster to which the subject belongs
    {
      LossSolReassigned[clust] = sum(ClusterLossCurSol_init)
      LossPerObjVector[clust] = subject_lossAll_init[subjectNr]
      templossPerClust[[clust]] = ClusterLossCurSol_init
    }
    else
    {
      #loss value per cluster
      templossPerClust[[clust]] = ClusterLossCurSol_init
      
      #loss value for cluster that looses subject subjectNr
      templossPerClust[[clust]][ partition[subjectNr] ] = LossClustMinussubject
      
      #loss value for cluster to which subject subjectNr is added
      cluster_subjects_index[[clust]] = sort( c( which( partition==clust ) , subjectNr ) )
      cluster = lapply( data , function(X){X[cluster_subjects_index[[clust]],]} )
      tempout = compute_single_CCA( cluster , component_count )
      templossPerClust[[clust]][clust] = tempout$loss$`cluster loss`
      LossPerObjVector[clust] =
        tempout$loss$`loss per subject`[cluster_subjects_index[[clust]]==subjectNr]
      LossPerObjExtraCluster[[clust]] = tempout$loss$`loss per subject`
      
      rm(cluster,tempout)
      
      LossSolReassigned[clust] = sum(templossPerClust[[clust]])
    }
  }
  
  #select cluster with lowest loss value
  tempopt_clust = which( LossSolReassigned==min(LossSolReassigned) )
  
  #when several clusters have the lowest loss, select one cluster at random
  if( length(tempopt_clust) != 1 )
  {
    nCl = length(tempopt_clust)
    opt_clust = tempopt_clust[ sample(1:nCl)[1] ]
  }
  else
  {
    opt_clust = tempopt_clust
  }
  
  #compute loss per subject for updated clustering
  subject_lossAll_update = subject_lossAll_init
  if( opt_clust != partition[subjectNr] )
  {
    #changes loss for subjects to which subject under consideration belonged
    subject_lossAll_update[ cluster_subjects_index_Minussubject ] = 
      subject_lossMinussubject
    
    #changes loss for subjects to which subject under consideration is reallocated
    subject_lossAll_update[ cluster_subjects_index[[opt_clust]] ] =
      LossPerObjExtraCluster[[opt_clust]]
  }
  
  Out = list()
  Out$opt_clust = opt_clust
  Out$opt_loss_elem = LossPerObjVector[opt_clust]
  Out$cluster_loss = templossPerClust[[opt_clust]]
  Out$subject_loss = subject_lossAll_update
  return(Out)
}