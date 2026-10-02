test_that("loss() returns the decay rate that closes the remaining gap", {
  expect_equal(loss(a = 20, b = 100, SVD = 50), 50 / 80)
})

test_that("calculate_visits() returns the expected structure", {
  out <- calculate_visits(a = 20, b = 100, SVD = 50)

  expect_named(out, c("optimal_loss", "c_parameter", "c_se"))
  expect_true(all(is.finite(unlist(out))))
  expect_gt(out$c_parameter, 0)
})

test_that("calculate_visits() discounts selfing when computing optimal_loss", {
  # baseline = 20, remaining gap = 80, discounted SVD = 30 -> loss = 0.375
  out <- calculate_visits(a = 20, b = 100, SVD = 50)

  expect_equal(out$optimal_loss, 30 / 80)
})

test_that("calculate_visits() reproduces the input curve when nothing decays", {
  # With SVD equal to b the whole remaining gap is filled by a single visit,
  # so there is no decay at all.
  out <- calculate_visits(a = 20, b = 100, SVD = 100)

  expect_equal(out$optimal_loss, 1)
})

test_that("calculate_visits() errors when SVD is below the selfing baseline", {
  expect_error(
    calculate_visits(a = 20, b = 100, SVD = 10),
    "negative"
  )
})

test_that("calculate_visits() errors when SVD overshoots the remaining gap", {
  # baseline = 20, remaining gap = 80, discounted SVD = 100 > 80
  expect_error(
    calculate_visits(a = 20, b = 100, SVD = 120),
    "exceed"
  )
})

test_that("calculate_visits() can plot", {
  expect_silent(calculate_visits(a = 20, b = 100, SVD = 50, plot_ = TRUE))
})

test_that("calculate_visits0() returns a single finite c", {
  out <- calculate_visits0(a = 20, b = 100, SVD = 50, loss = 0.5)

  expect_length(out, 1)
  expect_true(is.finite(out))
})

test_that("calculate_visits0() gives a larger c when returns decay faster", {
  # A steeper per-visit decay means more visits are needed to saturate, which
  # is reflected in a smaller fitted c.
  fast <- calculate_visits0(a = 20, b = 100, SVD = 50, loss = 0.9)
  slow <- calculate_visits0(a = 20, b = 100, SVD = 50, loss = 0.1)

  expect_lt(as.numeric(fast), as.numeric(slow))
})

test_that("calculate_visits0() validates its numeric arguments", {
  expect_error(calculate_visits0(a = "twenty", b = 100, SVD = 50), "numeric")
  expect_error(calculate_visits0(a = 20, b = "hundred", SVD = 50), "numeric")
})

test_that("calculate_visits0() can plot", {
  expect_silent(calculate_visits0(a = 20, b = 100, SVD = 50, loss = 0.625,
                                  plot_ = TRUE))
})