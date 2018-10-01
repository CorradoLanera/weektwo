context("test-fars_map_state")

test_that("error missing", {
    expect_error(fars_map_state(1, 2012),
        "does not exist"
    )
})

test_that("error nothing to drown", {
    expect_error(fars_map_state(2, 2015),
        "all regions out of bounds"
    )
})

test_that("NULL for pass", {
    expect_null(fars_map_state(1, 2013))
})
