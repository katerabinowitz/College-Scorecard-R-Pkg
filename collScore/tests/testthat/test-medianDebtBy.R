context("medianDebtBy")

testthat::test_that("medianDebtBy returns the expected objects with default bygroup", {
  data(scorecard13)
  result <- medianDebtBy(,scorecard13,c("Hampshire College","Amherst College"),)
  expect_is(result, "gg")
  expect_is(result, "ggplot")
})

testthat::test_that("medianDebtBy returns the expected objects with bygroup income", {
  data(scorecard13)
  result <- medianDebtBy(,scorecard13,c("Hampshire College","Amherst College"), "income")
  expect_is(result, "gg")
  expect_is(result, "ggplot")
})


