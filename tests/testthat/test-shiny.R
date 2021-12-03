test_that("iTReX works", {
  skip_on_cran()
  shinytest::expect_pass(test())
})
