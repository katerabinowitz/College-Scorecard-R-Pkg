context("getAllDataInCategory")

test_that("getAllDataInCategory validates the year argument correctly", {
  expect_error(getAllDataInCategory(categoryName = "earnings", year = 2014, pattern = "", addParams = "id,school.name"), "Incorrect year selection")
  expect_error(getAllDataInCategory(categoryName = "earnings", year = 1995, pattern = "", addParams = "id,school.name"), "Incorrect year selection")
})

test_that("getAllDataInCategory validates the categoryName argument correctly", {
  expect_error(getAllDataInCategory(categoryName = "earning", year = 2013, pattern = "", addParams = "id,school.name"), "Incorrect categoryName")
  expect_error(getAllDataInCategory(categoryName = "random category", year = 1996, pattern = "", addParams = "id,school.name"), "Incorrect categoryName")
})