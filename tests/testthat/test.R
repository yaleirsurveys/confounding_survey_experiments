context("Analysis code ran")

output <- survey4_analyze(images_directory = NULL)

test_that("analysis code", {
  expect_identical(output, expected = "All the code ran without error.")
})
