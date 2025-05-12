
test_that("generate_partial_clique works", {
  set.seed(10)
  res <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9)

  expect_true(is.list(res))
  expect_true(is.matrix(res$adj_mat))
  expect_true(all(dim(res$adj_mat) == c(10,10)))
})


test_that("generate_partial_clique returns correctly shaped and typed output", {
  out <- generate_partial_clique(
    n = 10,
    clique_fraction = 0.3,
    clique_edge_density = 0.5,
    seed = 123
  )

  # adj_mat exists, is a matrix, 10×10, numeric 0/1
  expect_true(is.list(out))
  expect_true(is.matrix(out$adj_mat))
  expect_equal(dim(out$adj_mat), c(10, 10))
  expect_true(all(out$adj_mat %in% c(0L, 1L)))

  # symmetry & diagonal
  expect_equal(out$adj_mat, t(out$adj_mat))
  expect_true(all(diag(out$adj_mat) == 1L))

  # clique_nodes length and edge_target consistency
  m <- round(10 * 0.3)
  expect_equal(length(out$clique_nodes), m)
  max_edges <- choose(m, 2)
  expect_gte(out$edge_target, round(0.5 * max_edges))
})

test_that("generate_partial_clique argument validation works", {
  # non-integer n
  expect_error(
    generate_partial_clique(n = 5.5, clique_fraction = 0.2, clique_edge_density = 0.2),
    "`n` must be a positive integer"
  )

  # invalid fraction
  expect_error(
    generate_partial_clique(n = 10, clique_fraction = 1.5, clique_edge_density = 0.2),
    "clique_fraction.*must be a single numeric in \\[0, 1\\]"
  )

  # invalid density
  expect_error(
    generate_partial_clique(n = 10, clique_fraction = 0.2, clique_edge_density = -0.1),
    "clique_edge_density.*must be a single numeric in \\[0, 1\\]"
  )
})
