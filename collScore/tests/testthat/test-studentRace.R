context("studentRace")

testthat::test_that("studentRace returns the expected objects", {
  data(scorecard13)
  result <- studentRace(,scorecard13,c("Yale University","Harvard University"),)
  expect_is(result, "gg")
  expect_is(result, "ggplot")
})