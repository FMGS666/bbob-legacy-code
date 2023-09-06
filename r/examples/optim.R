##
## optim.R - Example BBO Benchmark of the R optimizer optim() .
##
library("bbob")

my_optimizer <- function(par, fun, lower, upper, max_eval) {
  optim(par, fun, method="L-BFGS-B",
        lower=lower, upper=upper,
        control=list(maxit=max_eval))
}

budget <- 10000
bbo_benchmark(my_optimizer, "l-bfgs-b", "optim_l-bfgs-b",
              budget=10000)
