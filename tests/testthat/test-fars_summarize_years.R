context("test-fars_summarize_years")

test_that("two years", {
    fars_summarize_years(c(2013, 2014)) %>%
    expect_is("data.frame")
})


test_that("correct columns", {
    fars_summarize_years(c(2013, 2014)) %>%
    expect_named(c("MONTH", "2013", "2014"))
})


test_that("a known result", {
    fars_summarize_years(c(2013, 2014))[["2013"]][[2]] %>%
    expect_equal(1952)
})
