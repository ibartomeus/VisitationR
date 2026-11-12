test_that("V_transect_to_flower() scales the transect count", {
  expect_equal(V_transect_to_flower(V_transect = 100, 
                                    transect_size = 100, 
                                    flw_x_m2 = 10, 
                                    transect_time = 100, 
                                    lifespan = 200), 0.2)
})

test_that("V_transect_to_flower() returns 0 for 0 visits", {
  expect_equal(V_transect_to_flower(V_transect = 0, 
                                    transect_size = 100, 
                                    flw_x_m2 = 10, 
                                    transect_time = 100, 
                                    lifespan = 200), 0)
})

test_that("V_transect_to_flower() is vectorised over V_transect", {
  out <- V_transect_to_flower(V_transect = c(10, 20, 50), 
                              transect_size = 100, 
                              flw_x_m2 = 10, 
                              transect_time = 100, 
                              lifespan = 200)

  expect_length(out, 3)
  expect_equal(out, c(0.02, 0.04, 0.10))
})

