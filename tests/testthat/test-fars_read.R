context("test-fars_read")

readed_data <- fars_read("accident_2013.csv.bz2")

test_that("import works", {

    expect_is(readed_data, "data.frame")

    expect_equal(
        readed_data[["ST_CASE"]][[1]],
        10001L
    )

})
