context("Analysis code ran")

output <- survey4_analyze(survey_data = "data/confounding_democratic_peace_4_sim.sav", 
                          military_data = "data/NMC_v4_0.csv", 
                          trade_data = "data/dyadic_trade_3.0.csv")
test_that("analysis code", {
  expect_identical(output, expected = "All the code ran without error.")
})
