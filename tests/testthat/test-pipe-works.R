context("test-pipe-works")

test_that("simple pipe", {
  expect_equal(1 %>% sum(2), 3)
})
