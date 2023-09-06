##
## fgeneric.R - Low-level interface to the C fgeneric interface.
##

##' Setup a single BBOB experiment for a given function, trial id and dimension.
##'
##' These functions is part of the low-level BaBOB interface and not
##' meant for general use. Please see \code{\link{bbo_benchmark}} for
##' the recommended high-level interface.
##' 
##' @param algorithm_id [\code{character}]\cr Algorithm ID.
##'
##' @param datapath [\code{character}]\cr Base directory of
##' results. Note that this will append to already existing data in
##' the directory.
##'
##' @param function_id [\code{integer}]\cr ID of function under
##' test. The noise free functions are numbered 1 to 24 and the noisy
##' function 101 to 124.
##'
##' @param instance_id [\code{integer}]\cr Trial ID to use.
##'
##' @param dimension [\code{integer}]\cr Dimensionality of parameter
##' space.
##'
##' @param x [\code{numeric}]\cr Parameter setting at which to
##' evaluate the test function.
##'
##' @param reason [\code{character}]\cr Reason for restart.
##' 
##' @export
##' @useDynLib bbob do_bbob_setup_experiment
##' @rdname low-level.Rd
bbob_setup_experiment <- function(algorithm_id, datapath,
                                  function_id, instance_id, dimension) {
  full_path <- file.path(datapath, sprintf("data_f%i", function_id))  
  if (!file.exists(full_path))
    dir.create(full_path, recursive=TRUE)
  .Call("do_bbob_setup_experiment",
        as.character(algorithm_id), as.character(datapath), "",
        as.integer(function_id), as.integer(instance_id), as.integer(dimension))
}

##' @export
##' @useDynLib bbob do_bbob_end_experiment
##' @rdname low-level.Rd
bbob_end_experiment <- function()  {
  .Call("do_bbob_end_experiment")
}

##' @export
##' @useDynLib bbob do_bbob_f_eval
##' @rdname low-level.Rd
bbob_log_restart <- function(reason)
  .Call("do_bbob_log_restart", as.character(reason))

##' @export
##' @useDynLib bbob do_bbob_f_eval
##' @rdname low-level.Rd
bbob_f_eval <- function(x)
  .Call("do_bbob_f_eval", as.numeric(x))

##' @export
##' @useDynLib bbob do_bbob_n_evaluations
##' @rdname low-level.Rd
bbob_n_evaluations <- function()
  .Call("do_bbob_n_evaluations")

##' @export
##' @useDynLib bbob do_bbob_optimality_gap
##' @rdname low-level.Rd
bbob_optimality_gap <- function()
  .Call("do_bbob_optimality_gap")

##' One stop solution to run a BBOB experiment.
##'
##' @param optimizer [\code{function}]\cr Optimizer under test. The
##' exact function signature is \code{function(par, fun, lower, upper,
##' max_eval)}, were \code{par} is an initial search point, \code{fun}
##' is the function to minimize, \code{lower} and \code{upper} are the
##' lower and upper bounds of the box constrained search space and
##' \code{max_eval} is the maximum number of function evaluations the
##' optimizer should perform. It is safe to ignore the \code{par} if
##' your algorithm requires an initial population for example. You can
##' also use more function evaluations than specified in
##' \code{max_eval}. The setting is mainly provided for convenience
##' and as a last resort stopping criterion to limit the
##' runtime. Please see the Example section for an example of such a
##' wrapper function.
##' 
##' @param algorithm_id [\code{character}]\cr Short name of
##' algorithm. Should likely contain the package name containing the
##' optimization algorithm as well as the version used.
##'
##' @param data_directory [\code{character}]\cr Base directory for
##' result files. If it does not start with a '/' (or drive letter on
##' Windows), relative to the current working directory.
##'
##' @param dimensions [\code{integer}]\cr List of dimensions in which
##' to test the optimization algorithm. Default is fine for official
##' BBOB use.
##'
##' @param instances [\code{integer}]\cr What instances (trial IDs) to
##' use for the test. Default is fine for official BBOB use.
##'
##' @param replications [\code{integer}]\cr Number of replications to
##' perform. Defaults to 1 for BBOB 2012 compatibility but was 3 for
##' BBOB 2009. Default is fine for official BBOB use.
##'
##' @param budget [\code{integer}]\cr Maximal number of function
##' evaluations per run. If the desired precision is not reached and
##' the budget is not used up, a random independent restart will be
##' performed. Defaults to 10000000. You may want to lower this to
##' save some runtime at the expense of solving fewer function
##' instances.
##'
##' @param noisy [\code{logical}]\cr Wether to use the noisy test
##' functions (\code{noisy=TRUE}) or the noiseless test functions
##' (default, \code{noisy=FALSE}).
##' 
##' @return Nothing. Run for the side effect of creating a BBOB data
##' directory that can be used with the Python post-processing code.
##'
##' @examples
##' ## This is a minimal example for the built in L-BFGS-B optimizer.
##' \dontrun{
##' my_optimizer <- function(par, fun, lower, upper, max_eval) {
##'   optim(par, fun, method="L-BFGS-B",
##'         lower=lower, upper=upper,
##'         control=list(maxit=max_eval))
##' }
##'
##' budget <- 10000
##' bbo_benchmark(my_optimizer, "l-bfgs-b", "optim_l-bfgs-b",
##'                budget=10000)
##' }
##' @export
bbo_benchmark <- function(optimizer, algorithm_id, data_directory,
                          dimensions=c(2, 3, 5, 10, 20, 40),
                          instances=c(1:5, 41:50),
                          replications=1L,
                          budget=1e7,                       
                          noisy=FALSE) {
  
  function_ids <- if (noisy) 101:124 else 1:24
  nruns <- length(dimensions) * length(function_ids) 
  current_run <- 1
  pbar <- makeProgressBar(min=1, max=nruns)

  ## Start with the easy work (low dimensions) first:
  for (dimension in sort(dimensions, decreasing=FALSE)) {
    for (function_id in function_ids) {
      pbar$set(current_run)
      current_run <- current_run + 1
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
  }
}

##' Run BBOB timing experiment
##'
##' @param optimizer [\code{function}]\cr Optimizer under test. The
##' exact function signature is \code{function(par, fun, lower, upper,
##' max_eval)}, were \code{par} is an initial search point, \code{fun}
##' is the function to minimize, \code{lower} and \code{upper} are the
##' lower and upper bounds of the box constrained search space and
##' \code{max_eval} is the maximum number of function evaluations the
##' optimizer should perform. It is safe to ignore the \code{par} if
##' your algorithm requires an initial population for example. You can
##' also use more function evaluations than specified in
##' \code{max_eval} but this might increase the runtime of the timing
##' experiment unneccesarily.
##'
##' @param dimensions [\code{integer}]\cr List of dimensions in which
##' to time the optimization algorithm. Default is fine for official
##' BBOB use.
##'
##' @return Nothing. Run for the side effect of creating a BBOB data
##' directory that can be used with the Python post-processing code.
##'
##' @export
bbo_timing <- function(optimizer, dimensions=c(2, 3, 5, 10, 20, 40)) {
  res <- NULL
  for (dimension in dimensions) {
    runs <- 0
    ## Use function 8, instance 1 for timing:
    bbob_setup_experiment("bbob-timing", tempdir(), 8L, 1L, dimension)
    start_time <- proc.time()[3]
    repeat {
      runs <- runs + 1
      par <- runif(dimension, -4, 4)   
      optimizer(par, bbob_f_eval,
                lower=rep(-5, dimension),
                upper=rep( 5, dimension),
                100000L)
      end_time <- proc.time()[3]
      if (end_time - start_time > 30)
        break
    }
    eval_time <- (end_time - start_time) /  bbob_n_evaluations()
    res <- rbind(res, data.frame(Dimensions=dimension,
                                 runs=runs,
                                 time=eval_time))
    bbob_end_experiment()
  }
  res
}
