test_that("fetching projects works", {

  skip_on_ci()

  o <- openaire_projects_participants_kth()

  is_valid <- nrow(o) > 900
  expect_true(is_valid)
  
})
