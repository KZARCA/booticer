context("resampling")
library(dplyr)

n <- 1000
tab <- tibble(cost = runif(10000, 0, 50000), effect = rnorm(10000),
              strategy = c(rep("A", 3000), rep("B", 3000), rep("C", 4000)))
res <- get_resamples(tab, n)
m <- get_means(res)
d <- get_differences(m, "A")
plot_ce(d)

test_that("get_resamples returns a list of with adequate number of elements", {
  expect_length(res, 3)
  expect_length(res$A, n)
  expect_length(res$B, n)
  expect_length(res$C, n)
})


test_that("get_means returns a list of with adequate number of elements", {
  expect_length(m, 3)
  expect_length(res$A, n)
})

