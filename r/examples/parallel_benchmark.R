##
## parallel_benchmark.R - Run BBOB experiment in parallel
##

library("bbob")
library("snow")

bbo_benchmark_helper <- function(optimizer, algorithm_id,
                                 function_id, dimension,
                                 data_directory,
                                 instances=c(1:5, 41:50),
                                 replications=1L,
                                 budget=1e7,
                                 noisy=FALSE) {
  for (instance_id in instances) {
    for (run in 1:replications) {
      bbob_setup_experiment(algorithm_id, data_directory,
                            function_id, instance_id, dimension)
      while (bbob_n_evaluations() < budget
             && bbob_optimality_gap() > 1e-8) {
        if (bbob_n_evaluations() > 0)
          bbob_log_restart("independent restart")          
        par <- runif(dimension, -4, 4)
        optimizer(par, bbob_f_eval,
                  lower=rep(-5, dimension),
                  upper=rep( 5, dimension),
                  budget - bbob_n_evaluations())
      }
      bbob_end_experiment()
    }
  }   
}

parallel_bbo_benchmark <- function(cl, ## SNOW cluster object
                                   optimizer, algorithm_id, data_directory,
                                   dimensions=c(2, 3, 5, 10, 20, 40),
                                   instances=c(1:5, 41:50),
                                   replications=1L,
                                   budget=1e7,                       
                                   noisy=FALSE) {

  ## Make sure to load any other required packages / code to run
  ## optimizer on a cluster node.
  function_ids <- if (noisy) 101:124 else 1:24
  design <- expand.grid(function_id=function_ids, dimension=dimensions)
  clusterEvalQ(cl, library("bbob"))
  clusterExport(cl, "bbo_benchmark_helper")
  clusterExport(cl, "optimizer", environment())
  clusterExport(cl, "design", environment())
  clusterApplyLB(cl, 1:nrow(design), function(i) {
    function_id <- design$function_id[i]
    dimension <- design$dimension[i]
    bbo_benchmark_helper(optimizer, algorithm_id, function_id, dimension,
                         data_directory, instances, replications, budget, noisy)    
  })
}

my_optimizer <- function(par, fun, lower, upper, max_eval) {
  optim(par, fun, method="L-BFGS-B",
        lower=lower, upper=upper,
        control=list(maxit=max_eval))
}

## Run using local cluster with 16 cores:
cl <- makeSOCKcluster(rep("localhost", 16))
parallel_bbo_benchmark(cl, my_optimizer, "l-bfgs-b", "optim_l-bfgs-b",
                       budget=10000)
stopCluster(cl)
