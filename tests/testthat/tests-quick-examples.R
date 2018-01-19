context("werner")

test_that("Zelig", {
  explore_package("Zelig")
})

test_that("stats", {
  explore_package("stats")
})

test_that("dplyr", {
  adjacency_matrix("dplyr")
})

test_that("foreign", {
  adjacency_matrix("foreign")
})

test_that("devtools", {
  adjacency_matrix("devtools", coerce_to_matrix=TRUE)
})
