context("costByIncome")

testthat::test_that("costByIncome returns the expected objects when default type is used", {
  data(scorecard13)
  result <- costByIncome(dataset = scorecard13, year = 2013, schoolNames = c("University of Massachusetts-Lowell",
                                                                             "University of Arizona", "Alabama A & M University"))
  expect_is(result[[1]], "gg")
  expect_is(result[[1]], "ggplot")
})

testthat::test_that("costByIncome returns the expected objects when type is private", {
  data(scorecard13)
  result <- costByIncome(dataset = scorecard13, year = 2013, schoolNames = c("Massachusetts Institute of Technology",
                                                                   "Drake University", "Stanford University"), type = "private")
  expect_is(result[[1]], "gg")
  expect_is(result[[1]], "ggplot")
})