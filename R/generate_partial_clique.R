


###############################################################################
# generate_partial_clique.R
#   Create a random graph containing a dense *partial* clique.
#   All helpers + main function are self‑contained in this file.
###############################################################################

# ---------------------------------------------------------------------------
# Helper: validate a probability‑type scalar
# ---------------------------------------------------------------------------
.check_prob_scalar <- function(x, argname){
  if(!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x > 1)
    stop(sprintf("`%s` must be a single numeric in [0, 1].", argname),
         call. = FALSE)
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Main: generate_partial_clique()
# ---------------------------------------------------------------------------
#' Generate a random adjacency matrix with a dense partial clique
#'
#' @param n  Positive integer: number of nodes (matrix dimension).
#' @param clique_fraction  Fraction of nodes belonging to the clique (0–1).
#' @param clique_edge_density  Edge density *within* the clique (0–1).
#' @param outside_edge_density  Edge probability for every non‑clique pair
#'        (including clique–non‑clique pairs). Default 0.05.
#' @param seed  fixes seed.  Default 500
#' @param diag_one  Logical; if TRUE (default) place 1’s on the diagonal,
#'        otherwise leave 0’s (strict graph‑theory convention).
#'
#' @return List with five elements:
#'   \item{adj_mat}{Original n × n symmetric 0/1 matrix.}
#'   \item{adj_mat_ordered}{Same matrix permuted so clique block is upper‑left.}
#'   \item{node_order}{Permutation putting clique nodes first.}
#'   \item{clique_nodes}{Indices (original ordering) of clique nodes.}
#'   \item{edge_target}{# edges required inside the clique.}
#' @export


generate_partial_clique <- function(
    n,
    clique_fraction,
    clique_edge_density,
    outside_edge_density = 0.05,
    seed = 500,
    diag_one = TRUE
){

  # ---- 1. Basic argument checks -------------------------------------------
  if(!is.numeric(n) || length(n) != 1L || n <= 0 || n %% 1 != 0)
    stop("`n` must be a positive integer.", call. = FALSE)

  .check_prob_scalar(clique_fraction,     "clique_fraction")
  .check_prob_scalar(clique_edge_density, "clique_edge_density")
  .check_prob_scalar(outside_edge_density,"outside_edge_density")

  if(!is.logical(diag_one) || length(diag_one) != 1L)
    stop("`diag_one` must be TRUE or FALSE.", call. = FALSE)

  # ---- 2. Choose clique size & members ------------------------------------
  set.seed(seed)

  m <- round(n * clique_fraction)
  if(m < 2){
    warning("Clique size < 2; raised to 2 so it can contain edges.")
    m <- 2L
  }
  clique_nodes <- sample(seq_len(n), m)

  # ---- 3. Initialise empty adjacency matrix -------------------------------
  adj <- matrix(0L, nrow = n, ncol = n)

  # ---- 4. Populate required edges INSIDE the clique -----------------------
  pair_mat       <- utils::combn(clique_nodes, 2L)   # 2 × (m choose 2)
  total_possible <- ncol(pair_mat)
  edge_target    <- max(1L, round(clique_edge_density * total_possible))

  chosen_cols <- sample(seq_len(total_possible), edge_target)
  for(idx in chosen_cols){
    i <- pair_mat[1L, idx]
    j <- pair_mat[2L, idx]
    adj[i, j] <- 1L
    adj[j, i] <- 1L
  }

  # ---- 5. Sprinkle edges everywhere else ----------------------------------
  upper_mask <- upper.tri(adj, diag = FALSE) & (adj == 0L)
  n_upper    <- sum(upper_mask)

  if(n_upper > 0L && outside_edge_density > 0){
    adj[upper_mask] <- stats::rbinom(n_upper, 1L, outside_edge_density)
    adj <- pmax(adj, t(adj))   # force symmetry
  }

  # ---- 6. Diagonal convention --------------------------------------------
  diag(adj) <- as.integer(diag_one)

  # ---- 7. Create convenient ordering -------------------------------------
  node_order       <- c(clique_nodes, setdiff(seq_len(n), clique_nodes))
  adj_mat_ordered  <- adj[node_order, node_order, drop = FALSE]

  # ---- 8. Return ----------------------------------------------------------
  list(
    adj_mat          = adj,
    adj_mat_ordered  = adj_mat_ordered,
    node_order       = node_order,
    clique_nodes     = clique_nodes,
    edge_target      = edge_target
  )
}


