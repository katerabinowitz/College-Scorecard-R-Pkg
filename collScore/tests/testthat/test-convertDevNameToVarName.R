context("convertDevNameToVarName")

test_that("convertDevNameToVarName returns the correct variable name", {
  expect_match(convertDevNameToVarName("name"), "INSTNM")
  expect_match(convertDevNameToVarName("school.name"), "INSTNM")
  expect_match(convertDevNameToVarName("3_yr_repayment_suppressed.overall"), "RPY_3YR_RT_SUPP")
})