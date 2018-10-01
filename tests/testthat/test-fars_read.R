context("test-fars_read")

test_that("import works", {

    fars_read("accident_2013.csv.bz2") %>%
        expect_is("data.frame")

    fars_read("accident_2013.csv.bz2")[["ST_CASE"]][[1]] %>%
        expect_equal(10001L)

})
