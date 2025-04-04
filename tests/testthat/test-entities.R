test_that("projects request works", {
  res <- openaire("projects")
  is_valid <- nrow(res) >= 50
  expect_true(is_valid)
})

test_that("requesting publications from the API works", {
  res <- openaire("publications")
  is_valid <- (nrow(res) >= 50)
  expect_true(is_valid)
})

test_that("requesting research products from the API works", {
  res <- openaire("research")
  is_valid <- (nrow(res) >= 30)
  expect_true(is_valid)
})

test_that("requesting datasets from the API works", {
  res <- openaire("datasets")
  is_valid <- (nrow(res) >= 50)
  expect_true(is_valid)
})

test_that("requesting software from the API works", {
  res <- openaire("software")
  is_valid <- (nrow(res) == 50)
  expect_true(is_valid)
})

test_that("requesting other from the API works", {
  res <- openaire("other")
  is_valid <- (nrow(res) == 50)
  expect_true(is_valid)
})

test_that("requesting 10 projects in tsv format works", {

  # TODO: when requesting more than 6 entries, one gets one extra i.e. 7
  res <- 
    openaire("projects", params = api_params(
      proj_name = "KTH", 
      format = "tsv", 
      size = 10
    ))

  is_valid <- (nrow(res) >= 10)
  expect_true(is_valid)
})

# openaire("datasets", params = api_params(size = 10, format = "tsv"))

test_that("requesting xml works", {
  req <- function(entity) openaire(entity, params = api_params(size = 5, page = 1, format = "xml"))
  entities <- api_paths() |> names()
  res <- entities |> lapply(req)
  is_valid <- res |> lapply(function(x) nrow(x) >= 5) |> as.logical() |> all()
  expect_true(is_valid)
})

test_that("requesting json works", {
  req <- function(entity) openaire(entity, params = api_params(size = 5, page = 1, format = "json"))
  entities <- api_paths() |> names()
  res <- entities |> lapply(req)
  is_valid <- res |> lapply(function(x) lengths(x) == 3) |> as.logical() |> all()
  expect_true(is_valid)
})

# list(
#   openaire("projects", page_size = 10, params = api_params(proj_org = "KTH", size = 10, page = 2)),
#   openaire("projects", params = api_params(proj_org = "KTH", size = 10, page = 2)),
#   openaire("projects", params = api_params(proj_org = "KTH", size = 10, page = 0))
# )

