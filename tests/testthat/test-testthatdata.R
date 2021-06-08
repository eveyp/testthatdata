test_that("na's computed correctly", {
  test_data <- dplyr::tribble(
    ~no_na, ~na, ~all_na,
    1, NA, NA,
    2, 1, NA
  )
  expect_success(expect_no_na(test_data, no_na))
  expect_failure(expect_no_na(test_data, na))
  expect_failure(expect_no_na(test_data, all_na))
})

test_that("between works properly", {
  test_data <- dplyr::tribble(
    ~pass, ~too_low, ~too_high, ~both, ~tol_low, ~tol_high, ~tol_both,
    0, -1, 0, -1, -0.0000000001, 0, -0.0000000001,
    0.5, 0.5, 2, 2, 0.5, 1.0000000001, 1.0000000001
  )
  test_min <- 0
  test_max <- 1

  expect_success(expect_between(test_data, pass, test_min, test_max))
  expect_success(expect_between(test_data, tol_low, test_min, test_max))
  expect_success(expect_between(test_data, tol_high, test_min, test_max))
  expect_success(expect_between(test_data, tol_both, test_min, test_max))
  expect_failure(expect_between(test_data, too_low, test_min, test_max))
  expect_failure(expect_between(test_data, too_high, test_min, test_max))
  expect_failure(expect_between(test_data, both, test_min, test_max))
})
