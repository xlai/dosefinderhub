# Input checks (pages/global.R).

test_that("the default CRM skeleton passes the skeleton check", {
  d <- default_design
  skeleton <- dfcrm::getprior(halfwidth = 0.25 * d$ttl, target = d$ttl,
                              nu = d$crm$prior_mtd, nlevel = d$n_doses)
  expect_null(validate_skeleton(skeleton, d$n_doses))
})

test_that("the skeleton check catches each kind of bad skeleton", {
  expect_match(validate_skeleton(c("0.1", "0.2", "0.3"), 3), "number")
  expect_match(validate_skeleton(c(0.1, NA, 0.3), 3), "number")
  expect_match(validate_skeleton(c(0.1, 0.2), 3), "one prior DLT rate per dose")
  expect_match(validate_skeleton(c(0, 0.2, 0.3), 3), "between 0 and 1")
  expect_match(validate_skeleton(c(0.1, 0.2, 1), 3), "between 0 and 1")
  expect_match(validate_skeleton(c(0.1, 0.3, 0.2), 3), "increase")
  expect_match(validate_skeleton(c(0.1, 0.2, 0.2), 3), "increase")
})

test_that("the skeleton check waits until there is something to check", {
  expect_null(validate_skeleton(NULL, 5))
  expect_null(validate_skeleton(c(0.1, 0.2), NA))
})
