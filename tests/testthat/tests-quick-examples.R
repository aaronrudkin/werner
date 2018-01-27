context("werner")

test_that("Default built-in packages", {
  explore_package("stats")
  explore_package("base")
  explore_package("stats4")
  explore_package("datasets")
  explore_package("graphics")
  explore_package("tools")
  explore_package("utils")
})

test_that("Dependencies that we're loading anyway", {
  explore_package("rlang")
  explore_package("methods")
  explore_package("Matrix")
})

test_that("Dependencies that we're loading anyway 2", {
  explore_package("knitr")
})

test_that("Introspection", {
  explore_package("werner")
})

test_that("R6 example", {
  explore_package("testthat")
})
