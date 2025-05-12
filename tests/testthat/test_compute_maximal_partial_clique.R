
# 1. Simple known clique recovery (functional test)
test_that("compute_maximal_partial_clique recovers a full clique", {
  # Build a 5-node full clique
  A <- matrix(1L, nrow = 5, ncol = 5)
  diag(A) <- 1L

  # alpha = 1 means require 100% density
  res <- compute_maximal_partial_clique(A, alpha = 1)
  expect_setequal(res$clique, seq_len(5))
})

# 2. Partial clique recovery in a small graph (integration test)
test_that("recover partial clique of size 3 in 6-node graph", {
  A <- matrix(0L, 6, 6)
  diag(A) <- 1L
  # clique on nodes 2,4,6 fully connected
  pairs <- combn(c(2,4,6), 2)
  for(j in seq_len(ncol(pairs))) {
    i <- pairs[1,j]; k <- pairs[2,j]
    A[i,k] <- A[k,i] <- 1L
  }
  # alpha = 1 again, since our "partial" clique is full within those 3 nodes
  res <- compute_maximal_partial_clique(A, alpha = 1)
  expect_setequal(res$clique, c(2,4,6))
})


# 3. Extreme case 1: too small adjacency matrix (n < 5) should error
test_that("error if adjacency matrix has fewer than 5 nodes", {
  A4 <- diag(4)          # 4×4 identity: meets most checks but n < 5
  expect_error(
    compute_maximal_partial_clique(A4, alpha = 0.5),
    "`adj_mat` must have between 5 and 50 rows/columns"
  )
})

# 4. Error on non-symmetric input (error test)
test_that("error if adjacency matrix is not symmetric", {
  # 5×5 identity passes size, diag, 0/1 checks
  A <- diag(5)
  # introduce asymmetry: (1,2)=0, but (2,1)=1
  A[1,2] <- 0L
  A[2,1] <- 1L
  expect_error(
    compute_maximal_partial_clique(A, alpha = 0.5),
    "`adj_mat` must be symmetric"
  )
})

# 5. Error on alpha
test_that("error if alpha is outside [0.5, 1]", {
  A <- diag(5)
  # too low
  expect_error(
    compute_maximal_partial_clique(A, alpha = 0.4),
    "`alpha` must be between 0.5 and 1"
  )
  # too high
  expect_error(
    compute_maximal_partial_clique(A, alpha = 1.1),
    "`alpha` must be between 0.5 and 1"
  )
})
