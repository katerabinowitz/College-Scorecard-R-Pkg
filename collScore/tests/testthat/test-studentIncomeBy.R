context("studentIncomeBy")

testthat::test_that("studentIncomeBy returns the expected objects with bygroup dependent", {
  data(scorecard13)
  result <- studentIncomeBy(,scorecard13,c("Boston University","Northeastern University"),"dependent")
  expect_is(result, "gg")
  expect_is(result, "ggplot")
})

testthat::test_that("studentIncomeBy returns the expected objects with bygroup independent", {
  data(scorecard13)
  result <- studentIncomeBy(,scorecard13,c("Boston University","Northeastern University"),"independent")
  expect_is(result, "gg")
  expect_is(result, "ggplot")
})