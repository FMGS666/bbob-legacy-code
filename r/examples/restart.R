##
## restart.R - test restart logging.
##
library("bbob")

## Using low-level API:
bbob_setup_experiment("myalg", "myalg", 1, 1, 2)
par <- runif(2, -4, 4)
bbob_f_eval(par)
bbob_log_restart("my restart reason")

par <- runif(2, -4, 4)
bbob_f_eval(par)
bbob_log_restart("my other restart reason")

bbob_end_experiment()

## Using high-level API:
doptimizer <- function(par, fun, lower, upper, max_eval) {
  fun(par)
}

bbo_benchmark(doptimizer, "dopt", "dopt_restart", budget=20)
