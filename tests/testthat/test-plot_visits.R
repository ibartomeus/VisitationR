visits <- c(0, 2, 5, 10, 20, 40, 80)
seedset <- c(20, 45, 70, 88, 96, 99, 100)

test_that("the saturating curve is evaluated as documented", {
  a <- 20
  b <- 100
  c_val <- 0.1
  x <- 10

  expected <- (b * a) / 100 + (b - (b * a) / 100) * (1 - exp(-c_val * x))

  expect_equal(
    (b * a) / 100 + (b - (b * a) / 100) * (1 - exp(-c_val * x)),
    expected
  )
})

test_that("plot_visits() draws a curve without error", {
  expect_silent(plot_visits(a = 30, b = 200, c = 10, to_ = 20))
  expect_silent(plot_visits(a = 30, b = 200, c = 10, to_ = 20))
  expect_silent(plot_visits(a = 10, b = 150, c = 3, to_ = 20, add_ = TRUE))
})

test_that("plot_visits() returns the coordinates of the curve", {
  out <- plot_visits(a = 30, b = 200, c = 10, to_ = 20)

  expect_named(out, c("x", "y"))
  expect_length(out$x, 101)
  expect_true(all(out$y >= 0))
  expect_true(all(out$y <= 200))
})