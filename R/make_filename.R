#' Make fars filename
#'
#' Compose the correct name for a fars file given the reference year.
#'
#' @note This function is for internal usage only and it is not
#'       exported.
#'
#' @param year (num) The year of the interested fars file.
#'
#' @return A character string of length one with the filename
#'     (full extension included) of the fars file for the year of
#'     interest.
#'
#' @examples
#' \dontrun{
#'     weektwo:::make_filename("2012")
#' }
make_filename <- function(year) {
    if (!year == as.integer(year)) {
        stop(glue::glue("year must be an integer ",
                "(or an object cohercible to an integer)"
        ), call. = FALSE)
    }

    glue::glue("accident_{year}.csv.bz2")
}
