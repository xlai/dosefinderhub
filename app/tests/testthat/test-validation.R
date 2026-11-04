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

test_that("the BOIN check follows BOIN's own rules", {
  d <- default_design
  expect_null(validate_boin_phis(d$ttl, d$boin$phi_1, d$boin$phi_2))
  expect_null(validate_boin_phis(0.4, 0.6 * 0.4, 1.4 * 0.4))
  # The defaults of 0.15 and 0.35 no longer fit once the target is 0.4.
  expect_match(validate_boin_phis(0.4, 0.15, 0.35), "p.tox")
  expect_match(validate_boin_phis(0.4, 0.37, 0.56), "p.saf")
  expect_match(validate_boin_phis(0.25, 0, 0.35), "p.saf")
  expect_match(validate_boin_phis(0.5, 0.3, 1), "p.tox")
  expect_match(validate_boin_phis(0.7, 0.42, 0.9), "between 0.05 and 0.6")
  expect_null(validate_boin_phis(NA, 0.15, 0.35))
})

test_that("BOIN boundaries match BOIN::get.boundary() and convert back", {
  for (target in c(0.25, 0.4)) {
    p_saf <- 0.6 * target
    p_tox <- 1.4 * target
    lambda <- boin_lambdas(target, p_saf, p_tox)
    expected <- BOIN::get.boundary(target = target, ncohort = 10, cohortsize = 3,
                                   p.saf = p_saf, p.tox = p_tox)
    expect_equal(lambda$lambda_e, expected$lambda_e, tolerance = 1e-10)
    expect_equal(lambda$lambda_d, expected$lambda_d, tolerance = 1e-10)
    expect_equal(boin_probs(target, lambda$lambda_e, 1), p_saf, tolerance = 1e-3)
    expect_equal(boin_probs(target, lambda$lambda_d, 2), p_tox, tolerance = 1e-3)
  }
})
