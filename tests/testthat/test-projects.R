test_that("projects request works", {
  res <- openaire_projects()
  is_valid <- nrow(res) > 500
  expect_true(is_valid)
})
