test_that("compare_alpha returns a data.frame with correct structure for valid inputs", {
  # For alpha = 0.6 and trial = 1, we expect:
  #   - 1 alpha × 1 trial × 15 implementations = 15 rows
  #   - 4 columns named "trial", "alpha", "implementation", "outcome"
  res <- compare_alpha(c(0.6), trial = 1)
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 15)
  expect_equal(
    names(res),
    c("trial", "alpha", "implementation", "outcome")
  )
  
  # All rows should have alpha == 0.6 and trial == 1
  expect_true(all(res$alpha == 0.6))
  expect_true(all(res$trial == 1))
})

test_that("compare_alpha throws an error when alpha or trial is out of bounds", {

  
  # alpha > 1
  expect_error(
    compare_alpha(alpha = c(0.6, 1.1), trial = 1),
    regexp = "alpha.*between 0.5 and 1"
  )
  
  # trial < 1
  expect_error(
    compare_alpha(alpha = 0.6, trial = 0),
    regexp = "trial.*between 1 and 15"
  )
  
  # trial > 15
  expect_error(
    compare_alpha(alpha = 0.6, trial = 16),
    regexp = "trial.*between 1 and 15"
  )
  

})