test_that("aux_mean", {
  expect_equal(aux_mean(10,0.3), 3)
  expect_equal(aux_mean(10,0.5), 5)
  expect_equal(aux_mean(10,0.7), 7)
})

test_that("bin_probability", {
  expect_equal(bin_probability(success = 3, trials = 5, prob = 0.5), 0.3125)
  expect_error(bin_probability(11,5,0.5))
  expect_equal(bin_probability(success = 3, trials = 25, prob = 0.3), 0.02427999)
})

test_that("bin_distribution", {
  expect_is(bin_distribution(5,0.1), "bindis")
  expect_length(bin_distribution(5,0.8), 2)
})

test_that("bin_choose", {
  expect_equal(bin_choose(10,6), 210)
  expect_error(bin_choose(4,9))
  expect_equal(bin_choose(5, 1:5), c(5,10,10,5,1))
})

test_that("bin_cumulative", {
  expect_is(bin_cumulative(10,0.3), "bincum")
  expect_length(bin_cumulative(25,0.8), 3)
  expect_is(bin_cumulative(10,0.4), "bincum")
})

test_that("aux_variance", {
  expect_equal(aux_variance(10,0.4), 2.4)
  expect_equal(aux_variance(20,0.2), 3.2)
  expect_equal(aux_variance(20,0.6), 4.8)
})

test_that("aux_mode", {
  expect_equal(aux_mode(10,0.7), 7)
  expect_equal(aux_mode(10,0.2), 2)
  expect_equal(aux_mode(40,0.6), 24)
})

test_that("aux_skewness", {
  expect_equal(aux_skewness(10,0.5), 0)
  expect_equal(aux_skewness(25,0.9), -0.5333333333333333)
  expect_equal(aux_skewness(30,0.5), 0)
})

test_that("aux_kurtosis", {
  expect_equal(aux_kurtosis(10,0.4), -0.18333333333333333)
  expect_equal(aux_kurtosis(10,0.5), -0.2)
  expect_equal(aux_kurtosis(40,0.2), 0.00625)
})

test_that("check_prob", {
  expect_error(check_prob(prob = -24))
  expect_true(check_prob(prob = 0.9))
  expect_true(check_prob(prob = 1))
})

test_that("check_trials", {
  expect_true(check_trials(trials = 50))
  expect_error(check_trials(trials = 50.5))
  expect_error(check_trials(trials = -300))
})


test_that("check_success", {
  expect_true(check_success(success = c(1,2,3,5),trials = 130))
  expect_error(check_success(success = -24, trials = 15))
  expect_error(check_success(success = 50, trials = 20))
})


