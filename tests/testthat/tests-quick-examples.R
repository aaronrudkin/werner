context("werner")

test_that("base has a bunch of weird edge cases", {
  skip_on_cran()
  explore_package("base")
})

test_that("datasets doesn't export any functions", {
  explore_package("datasets")
})

test_that("tools has some weird edge cases in BibTex", {
  skip_on_cran() # Slow
  explore_package("tools")
})

test_that("utils has some unusual edge cases as well.", {
  skip_on_cran() # Slow
  explore_package("utils")
})

test_that("Default built-in packages", {
  explore_package("stats")
  explore_package("stats4")
  explore_package("graphics")

  expect_null(werner:::diagnose_werner_failures("stats4"))
})

test_that("Dependencies that we're loading anyway", {
  explore_package("rlang")
  adjacency_matrix("Matrix")

  # Some unsupported items in `methods`
  werner:::diagnose_werner_failures("methods")
})

test_that("Dependencies that we're loading anyway 2", {
  adjacency_matrix("knitr", coerce_to_matrix = TRUE)
})

test_that("Introspection", {
  explore_package("werner", show_progress=TRUE)
})

test_that("R6 example", {
  explore_package("testthat")
})

test_that("Skip if not installed example.", {
  skip_if_not_installed("Zelig")
  explore_package("Zelig")
})

test_that("Self-flattery", {
  skip_if_not_installed("fabricatr")
  z = werner:::diagnose_werner_failures("fabricatr")
  expect_equal(length(z), 0)
})

test_that("Fail on fake package.", {
  expect_error(explore_package("not_a_real_package"))
})

test_that("Plotting method", {
  plot_adjacency_matrix(adjacency_matrix("werner"))
})
