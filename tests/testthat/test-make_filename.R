context("test-make_filename")

test_that("correct filename", {

    make_filename(2012) %>%
        expect_equal("accident_2012.csv.bz2")

})
