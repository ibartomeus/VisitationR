test_that("V_transect_to_flower() scales the transect count", {
  expect_equal(V_transect_to_flower(50, flw_x_m2 = 100, lifespan = 8), 400)
})

test_that("V_transect_to_flower() returns 0 for 0 visits", {
  expect_equal(V_transect_to_flower(0, flw_x_m2 = 100, lifespan = 8), 0)
})

test_that("V_transect_to_flower() is vectorised over V_transect", {
  out <- V_transect_to_flower(c(10, 20, 50), flw_x_m2 = 100, lifespan = 8)

  expect_length(out, 3)
  expect_equal(out, c(80, 160, 400))
})

test_that("V_transect_to_flower() increases with density and lifespan", {
  sparse <- V_transect_to_flower(50, flw_x_m2 = 10, lifespan = 8)
  dense <- V_transect_to_flower(50, flw_x_m2 = 1000, lifespan = 8)
  long <- V_transect_to_flower(50, flw_x_m2 = 100, lifespan = 16)

  expect_gt(sparse, dense)
  expect_gt(long, V_transect_to_flower(50, flw_x_m2 = 100, lifespan = 8))
})