context("test-fars_read_years")

test_that("single year", {
    expect_is(fars_read_years(2013)[[1]],
        "data.frame"
    )
})


test_that("two year", {
    expect_length(fars_read_years(c(2013, 2014)), 2)
    expect_is(fars_read_years(c(2013, 2014))[[1]],
        "data.frame"
    )
    expect_is(fars_read_years(c(2013, 2014))[[2]],
        "data.frame"
    )
})

test_that("character", {
    expect_is(fars_read_years("2013")[[1]],
        "data.frame"
    )
})


test_that("warning for missing years", {
    expect_warning(fars_read_years(2012))
})


test_that("NULL for missing years", {
    expect_null(
        suppressWarnings(fars_read_years(2012)[[1]]
        )
    )
})


test_that("correct columns", {
    expect_length(fars_read_years(2013)[[1]], 2)

    expect_equal(
        names(fars_read_years(2013)[[1]]),
        c("MONTH", "YEAR")
    )
})
