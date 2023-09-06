##' Naive implementation of pure random search.
##'
##' @param par Initial parameter combination (ignored).
##' @param fn Function to minimize.
##' @param lower Lower bounds of search space.
##' @param upper Upper bounds of search space.
##' @param max_eval Maximium number of function evaluations to perform.
##'
##' @return Best solution found.
##' 
##' @export
random_search <- function(par, fn, lower, upper, max_eval) {
  dim <- length(upper)
  best_par <- NULL
  best_value <- Inf
  for (i in 1:max_eval) {
    par <- runif(dim, min=lower, max=upper)
    value <- fn(par)
    if (value < best_value) {
      best_par <- par
      best_value <- value
    }      
  }
  list(par=best_par, value=best_value)
}
