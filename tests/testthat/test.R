context("Analysis code ran")

output <- survey4_analyze()
test_that("analysis code", {
  expect_identical(output, expected = "All the code ran without error.")
})
