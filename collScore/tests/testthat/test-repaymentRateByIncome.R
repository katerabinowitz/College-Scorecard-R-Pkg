context("repaymentRateByIncome")

testthat::test_that("repaymentRateByIncome returns the expected objects", {
  data(scorecard13)
  result <- repaymentRateByIncome(dataset = scorecard13, year = 2013, schoolNames = c("University of Massachusetts-Lowell", "Drake University"))
  expect_is(result[[1]], "gg")
  expect_is(result[[1]], "ggplot")
})

testthat::test_that("repaymentRateByIncome displays the correct message when data for a school in the supplied list is not available", {
  data(scorecard13)
  result <- repaymentRateByIncome(dataset = scorecard13, year = 2013, schoolNames = c("University of Massachusetts-Lowell", "Holyoke Community College"))
  expect_message(repaymentRateByIncome(dataset = scorecard13, year = 2013, schoolNames = c("University of Massachusetts-Lowell", "Holyoke Community College")), 
                 "No data is available for the selected school Holyoke Community College therefore data for this school will not be displayed")
  expect_is(result[[1]], "gg")
  expect_is(result[[1]], "ggplot")
})

testthat::test_that("repaymentRateByIncome returns null when data is not available for any of the supplied school(s)", {
  data(scorecard13)
  result <- repaymentRateByIncome(dataset = scorecard13, year = 2013, schoolNames = c("unknown College1", "Unknown College2"))
  expect_equal(result, NULL)
})