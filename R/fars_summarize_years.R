#' Summarize fars record by year
#'
#' Compute the number of records are present for fars for each year
#' (columns) and month (rows).
#'
#' @note If a year not present in the data is provided, a warning is
#'       returned and no column is created for it.
#'
#' @inheritParams fars_read_years
#'
#' @return A data frame with a number of column equals to the
#'         number of (valid) years passed as input (plus one for the
#'         months) and one row each month included in the corresponding
#'         fars files. The data frame is filled with the number
#'         of records included for the corresponding year and month.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'    file_path <- system.file("tests", "testthat", package = "weektwo")
#'
#'    fars_summarize_years(c(2013, 2014), path = file_path)
#'
#'    fars_summarize_years(c(2012, 2013, 2014), path = file_path)
#' }
fars_summarize_years <- function(years, path = ".") {

    years <- suppressWarnings(as.numeric(years))
    assertive::assert_all_are_whole_numbers(years)

    dat_list <- fars_read_years(years, path = path)

    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by_("YEAR", "MONTH") %>%
        dplyr::summarize_("n" = "n()") %>%
        tidyr::spread_("YEAR", "n")
}
