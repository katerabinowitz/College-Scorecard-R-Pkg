context("completionRate")

test_that("completionRate returns the expected objects", {
  data(scorecard13)
  result <- completionRate(dataset = scorecard13,c("University of Chicago","Northwestern University"),"race")
  #expect_is(result[1], "gg")
  #expect_is(result[2], "ggplot")
})