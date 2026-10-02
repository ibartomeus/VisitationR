test_that("pseudo_R2() is 1 for a perfect fit", {
  x <- seq(0, 60, length.out = 25)
  y <- 20 + 80 * (1 - exp(-0.25 * x))
  model <- fit_data(seedset = y, visitation = x, simplify = "model")

  expect_equal(pseudo_R2(model = model, seedset = y), 1, tolerance = 1e-6)
})

test_that("pseudo_R2() equals 1 - RSS/TSS by definition", {
  model <- fit_data(seedset = seedset, visitation = visits, simplify = "model")

  rss <- sum(residuals(model)^2)
  tss <- sum((seedset - mean(seedset))^2)

  expect_equal(pseudo_R2(model, seedset), 1 - rss / tss)
})

test_that("pseudo_R2() returns a single value strictly between 0 and 1 for real data", {
  model <- fit_data(seedset = seedset, visitation = visits, simplify = "model")

  out <- pseudo_R2(model = model, seedset = seedset)

  expect_length(out, 1)
  expect_type(out, "double")
  expect_true(out > 0 && out < 1)
})

test_that("pseudo_R2() is much lower for unstructured data", {
  set.seed(42)
  x <- seq(0, 60, length.out = 40)

  structured <- 20 + 80 * (1 - exp(-0.25 * x)) + stats::rnorm(40, sd = 2)
  random <- stats::runif(40, 20, 100)

  r2_structured <- pseudo_R2(
    fit_data(seedset = structured, visitation = x, simplify = "model"),
    seedset = structured
  )
  r2_random <- pseudo_R2(
    fit_data(seedset = random, visitation = x, simplify = "model"),
    seedset = random
  )

  expect_gt(r2_structured, 0.9)
  expect_lt(r2_random, r2_structured)
})