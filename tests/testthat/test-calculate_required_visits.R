test_that("calculate_required_visits() returns 0 when selfing is enough", {
  # baseline = 96 > target 0.95 * 100
  expect_equal(calculate_required_visits(c_val = 0.1, b = 100, a = 96), 0)

  # the same, expressed by lowering the target
  expect_equal(
    calculate_required_visits(c_val = 0.1, b = 100, a = 50,
                              target_percent = 0.4),
    0
  )
})

test_that("calculate_required_visits() inverts the model exactly", {
  a <- 20
  b <- 100
  c_val <- 0.1
  target <- 0.95

  x <- calculate_required_visits(
    c_val = c_val, b = b, a = a,
    target_percent = target
  )

  seedset_at_x <- (b * a) / 100 +
    (b - (b * a) / 100) * (1 - exp(-c_val * x))

  expect_equal(seedset_at_x, b * target, tolerance = 1e-10)
})

test_that("calculate_required_visits() agrees with the closed-form solution", {
  a <- 20
  b <- 100
  c_val <- 0.1
  target <- 0.95

  baseline <- (b * a) / 100
  expected <- -log(1 - ((b * target - baseline) / (b - baseline))) / c_val

  expect_equal(
    calculate_required_visits(c_val, b, a, target_percent = target),
    expected
  )
})

test_that("calculate_required_visits() decreases as c increases", {
  slow <- calculate_required_visits(c_val = 0.01, b = 100, a = 20)
  fast <- calculate_required_visits(c_val = 0.5, b = 100, a = 20)

  expect_gt(slow, fast)
  expect_gt(fast, 0)
})

test_that("calculate_required_visits() round-trips the parameter from calculate_visits()", {
  my_c <- calculate_visits(a = 20, b = 100, SVD = 35)$c_parameter
  x <- calculate_required_visits(c_val = my_c, b = 100, a = 20)

  expect_true(x > 0)
  expect_true(x < Inf)
})