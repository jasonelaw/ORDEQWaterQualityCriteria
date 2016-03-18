context("metalCriteria")
hardness <- c(25, 125, 225, 325, 400)
tol      <- 1e-5

test_that("Cadmium criteria correct", {
  expect_equal(metalCriteria(hardness, 'cadmium', toxicity = 'acute'),
               c(0.821101892407,5.044698529249,9.789998014042,14.822626126205,18.734598642374))
  expect_equal(metalCriteria(hardness, 'cadmium', toxicity = 'chronic'),
               c(0.0936968237,0.2872405824,0.4318577787,0.5571341111,0.6432217364))
})

test_that("Chromium criteria correct", {
  expect_equal(metalCriteria(hardness, 'chromium', toxicity = 'acute'),
               c(183.0659069317,684.0122901081,1106.9604047590,1495.9844422769,1773.2980532507))
  expect_equal(metalCriteria(hardness, 'chromium', toxicity = 'chronic'),
               c(23.8131133690,88.9759457843,143.9928059534,194.5968406638,230.6696439922))
})

test_that("Copper criteria correct", {
  expect_equal(metalCriteria(hardness, 'copper', toxicity = 'acute'),
               c(4.801002749255,21.872654492311,38.055659013185,53.813269771962,65.441583928317))
  expect_equal(metalCriteria(hardness, 'copper', toxicity = 'chronic'),
               c(3.616582041387,14.307646126444,23.642780254564,32.371506870310,38.656172142951))
})

test_that("Lead criteria correct", {
  expect_equal(metalCriteria(hardness, 'lead', toxicity = 'acute'),
               c(13.8821727935,82.2705689542,154.2302894611,226.6880806307,280.8464812000))
  expect_equal(metalCriteria(hardness, 'lead', toxicity = 'chronic'),
               c(0.5409683439,3.2059659610,6.0101329607,8.8337090591,10.9441841772))
})

test_that("Nickel criteria correct", {
  expect_equal(metalCriteria(hardness, 'nickel', toxicity = 'acute'),
               c(144.9178376852,565.5232558349,929.8459596043,1269.1645160390,1512.8899943659))
  expect_equal(metalCriteria(hardness, 'nickel', toxicity = 'chronic'),
               c(16.0958977086,62.8121742856,103.2771789150,140.9649947302,168.0353708192))
})

test_that("Silver criteria correct", {
  expect_equal(metalCriteria(hardness, 'silver', toxicity = 'acute'),
               c(0.2963978881,4.7217556900,12.9769377435,24.4262991070,34.9109345676))
})

test_that("Zinc criteria correct", {
  expect_equal(metalCriteria(hardness, 'zinc', toxicity = 'acute'),
               c(36.2017651055,141.5686304956,232.9482350791,318.1075292888,379.2980477944))
  expect_equal(metalCriteria(hardness, 'zinc', toxicity = 'chronic'),
               c(36.4978940634,142.7266561029,234.8537421145,320.7096358679,382.4006903121))
})

