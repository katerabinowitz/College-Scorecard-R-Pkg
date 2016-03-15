context("getData")

test_that("getData returns an error if no apiKey is supplied", {
  expect_error(getData(fieldParams = "school.degrees_awarded.predominant=2,3", optionParams = "_fields=id,school.name,2013.student.size"), "An API Key is required to access the CollegeScoreCard API.")
})