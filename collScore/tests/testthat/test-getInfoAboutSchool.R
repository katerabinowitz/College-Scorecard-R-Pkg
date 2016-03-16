context("getInfoAboutSchool")

testthat::test_that("getInfoAboutSchool returns the expected objects", {
  data(scorecard13)
  result <- getInfoAboutSchool(,scorecard13, schoolNames = c("Judson College","Birmingham Southern College", "University of Alaska Fairbanks"))
  expect_is(result, "data.frame")
})