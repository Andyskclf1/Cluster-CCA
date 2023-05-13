# extensive simulation study to evaluate performance of 2D Cluster-CCA 
# algorithm under different parameter configurations

rm(list = ls())
source("cluster_CCA_multistart.R")
source("sim_data_mixedCCA.R")
source("get_results.R")


# algorithm parameters
component_count = 1
cluster_count = 2
step_count = 100
multi_step_count = 5
tolerance = 0.0001

data_rep_count = 3

# test different parameters
pars_list = list("subject_count" = c(100, 150, 200),
                 "variable_count" = c(5, 10, 15), 
                 "max_weight" = c(0.2, 0.6, 1),
                 "correlation" = c(0.3, 0.6, 0.9),
                 "noise" = c(0.2, 0.1, 0.01)
)
pars_grid = expand.grid(pars_list)    
runs = list()
runs_optimal_start = list()
test_index = 1
for (data_rep in 1:data_rep_count){
  for (pars_index in 1:nrow(pars_grid)){
    data = sim_data(subject_count = pars_grid$subject_count[pars_index], 
                    variable_count = pars_grid$variable_count[pars_index], 
                    max_weight = pars_grid$max_weight[pars_index], 
                    correlation = pars_grid$correlation[pars_index],
                    noise = pars_grid$noise[pars_index])
    
    
    subject_count = pars_grid$subject_count[pars_index]
    original_p = c(rep(1, subject_count/2), rep(2, subject_count/2))
    
    
    run_optimal_start = cluster_CCA( cluster_count = cluster_count ,
                                      component_count = component_count ,
                                      data = data$data ,
                                      center = F , tolerance = tolerance ,
                                      verbose = F, 
                                      ALS_iteration_count = step_count ,
                                      show_info = F , 
                                      initial_partition = original_p)
    
    run = cluster_CCA_multi(multi_iteration_count = multi_step_count, 
                             ALS_iteration_count = step_count, 
                             cluster_count = cluster_count, 
                             component_count = component_count,
                             data = data$data, 
                             center = F, tolerance = tolerance)
    
    
    runs[[test_index]] = list('run' = run,
                                 'parameters' = pars_grid[pars_index,],
                                 'data' = data)
    
    runs_optimal_start[[test_index]] = list('run' = run_optimal_start,
                                               'parameters' = pars_grid[pars_index,],
                                               'data' = data)
    print(test_index)
    test_index = test_index + 1
  }
}; rm(run , data, pars_index)

# results
ari = sapply(runs, function(x){get_results(x)$adjustedRandIndex})
max_ari_run = runs[[which.max(ari)]]
mean_ari = mean(ari)
max_ari = max(ari)

tuckers = sapply(runs, function(x){get_results(x)$Tuckers})
max_tuckers_run = runs[[which.max(tuckers)]]
mean_tuckers = mean(tuckers)
max_tuckers = max(tuckers)
results_table = cbind(rbind(pars_grid, pars_grid, pars_grid), ari, tuckers)

