visits <- c(0, 2, 5, 10, 20, 40, 80)
seedset <- c(20, 45, 70, 88, 96, 99, 100)

test_that("fit_data() returns named coefficients by default", {
  fit <- fit_data(seedset = seedset, visitation = visits)

  expect_named(fit, c("a", "b", "c"))
  expect_true(all(is.finite(fit)))
})

test_that("fit_data() recovers the parameters of a generated curve", {
  truth <- list(a = 10, b = 120, c = 0.25)
  x <- seq(0, 60, length.out = 25)
  y <- with(truth, {
    (b * a) / 100 + (b - (b * a) / 100) * (1 - exp(-c * x))
  })

  fit <- fit_data(seedset = y, visitation = x)

  expect_equal(unname(fit[["a"]]), truth$a, tolerance = 1e-3)
  expect_equal(unname(fit[["b"]]), truth$b, tolerance = 1e-2)
  expect_equal(unname(fit[["c"]]), truth$c, tolerance = 1e-4)
})

test_that("fit_data() fits a noisy saturating curve well", {
  set.seed(1)
  x <- seq(0, 60, length.out = 30)
  y <- 20 + 80 * (1 - exp(-0.2 * x)) + stats::rnorm(30, sd = 2)

  fit <- fit_data(seedset = y, visitation = x)
  model <- fit_data(seedset = y, visitation = x, simplify = "model")

  expect_gt(unname(fit[["c"]]), 0)
  expect_gt(pseudo_R2(model = model, seedset = y), 0.95)
})

test_that("fit_data() with simplify = 'stats' returns a coefficient matrix", {
  out <- fit_data(seedset = seedset, visitation = visits, simplify = "stats")

  expect_true(is.matrix(out))
  expect_equal(rownames(out), c("a", "b", "c"))
  expect_true(all(out[, "Std. Error"] > 0))
})

test_that("fit_data() with simplify = 'model' returns an nls object", {
  model <- fit_data(seedset = seedset, visitation = visits, simplify = "model")

  expect_s3_class(model, "nls")
  expect_true(all(c("a", "b", "c") %in% names(coef(model))))
})

test_that("fit_data() accepts explicit starting values", {
  fit_default <- fit_data(seedset = seedset, visitation = visits)
  fit_manual <- fit_data(seedset = seedset, visitation = visits,
                         a_start = 15, b_start = 105, c_start = 0.15)

  expect_equal(fit_default, fit_manual, tolerance = 1e-4)
})

test_that("fit_data() validates its inputs", {
  expect_error(fit_data(seedset = "a", visitation = visits), "numeric")
  expect_error(fit_data(seedset = c(1, 2), visitation = visits), "same length")
  expect_error(
    fit_data(seedset = c(NA, seedset[-1]), visitation = visits),
    "missing"
  )
  expect_error(
    fit_data(seedset = seedset, visitation = visits, simplify = "nope"),
    "should be one of"
  )
})