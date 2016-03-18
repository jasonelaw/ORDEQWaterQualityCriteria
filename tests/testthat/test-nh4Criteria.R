context('Ammonia criteria')

test_that("Acute, salmonids present values correct", {
  pH <- seq(6.5, 9, by = 0.5)
  expect_that(signif(nh4Criteria(pH, 10, toxicity = 'acute', salmonids = TRUE), 2), 
              equals(c(33, 24, 13, 5.6, 2.1, 0.88)))
  expect_that(signif(nh4Criteria(pH, 20, toxicity = 'acute', salmonids = TRUE), 2), 
              equals(c(23, 17, 9.2, 3.9, 1.5, 0.62)))
  expect_that(signif(nh4Criteria(pH, 30, toxicity = 'acute', salmonids = TRUE), 2), 
              equals(c(9.9, 7.3, 4.0, 1.7, 0.65, 0.27)))
})

test_that("Acute, salmonids present values correct", {
  pH <- seq(6.5, 9, by = 0.5)
  expect_that(signif(nh4Criteria(pH, 10, toxicity = 'acute', salmonids = FALSE), 2), 
              equals(c(51, 38, 21, 8.8, 3.3, 1.4)))
  expect_that(signif(nh4Criteria(pH, 20, toxicity = 'acute', salmonids = FALSE), 2), 
              equals(c(23, 17, 9.2, 3.9, 1.5, 0.62)))
  expect_that(signif(nh4Criteria(pH, 30, toxicity = 'acute', salmonids = FALSE), 2), 
              equals(c(9.9, 7.3, 4.0, 1.7, 0.65, 0.27)))
})

test_that("Chronic values correct", {
  pH <- seq(6.5, 9, by = 0.5)
  expect_that(signif(nh4Criteria(pH, 5, toxicity = 'chronic'), 2),
              equals(c(4.9, 4.4, 3.2, 1.8, 0.8, 0.36)))
  expect_that(signif(nh4Criteria(pH, 20, toxicity = 'chronic'), 2), 
              equals(c(2.1, 1.9, 1.4, 0.78, 0.35, 0.16)))
  expect_that(signif(nh4Criteria(pH, 30, toxicity = 'chronic'), 2), 
              equals(c(1.1, 0.99, 0.73, 0.41, 0.18, 0.081)))
})

test_that("Bad pH values raise error.", {
  expect_error(nh4Criteria(-1, 10))
  expect_error(nh4Criteria(15, 10))
})

test_that("Bad temperature values raise error.", {
  expect_error(nh4Criteria(7, -5))
  expect_error(nh4Criteria(7, 35))
})
