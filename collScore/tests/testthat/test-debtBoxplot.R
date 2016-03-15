context("debtBoxplot")

testthat::test_that("debtBoxplot returns the expected objects", {
  data(scorecard13)
  result <- debtBoxplot(,scorecard13,c("New York University","Cornell University"))
  expect_is(result, "gg")
  expect_is(result, "ggplot")
})