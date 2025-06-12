



#' compare_alpha
#'
#' @param alpha vector of values for alpha
#' @param trial number of trials
#'
#' @return dataset with results to know whether or not the provided clique_idx forms a valid partial clique.
#' @export

compare_alpha <- function(alpha, trial){
  # Input validation
  if (!is.numeric(alpha) || any(alpha < 0.5 | alpha > 1)) {
    stop("Error: 'alpha' must be numeric and each value must be between 0.5 and 1.")
  }
  if (!is.numeric(trial) || length(trial) != 1 || trial < 1 || trial > 15) {
    stop("Error: 'trial' must be a single numeric value between 1 and 15.")
  }

  results <- data.frame()

  for (k in seq_along(alpha)) {
    message("alpha = ", alpha[k])

    for (i in seq_len(trial)) {
      message("trial = ", i)
      set.seed(trial)  # to freeze the randomness of adj_mat

      # generate the data
      data <- UWBiost561::generate_partial_clique(
        n = 10,
        clique_fraction = 0.9,
        clique_edge_density = 0.9
      )
      adj_mat <- data$adj_mat

      for (f in 1:15) {
        message("function = ", f)
        set.seed(trial)

        result <- UWBiost561::compute_maximal_partial_clique_master(
          adj_mat     = adj_mat,
          alpha       = alpha[k],
          number      = f,
          time_limit  = 30
        )

        results_mat <- matrix(NA, nrow = 1, ncol = 4)
        results_mat[1, ] <- c(i, alpha[k], f, result$valid)
        results_mat <- as.data.frame(results_mat)

        results <- data.table::rbindlist(
          list(results, results_mat),
          use.names = TRUE,
          fill = TRUE
        )
      }
    }
  }

  names(results) <- c("trial", "alpha", "implementation", "outcome")
  return(results)
}

