#' Read multiple fars
#'
#' Import a set of fars files given the vector of their years.
#'
#' @details Computation does not stop if the file does not exist: a
#'    warning is produced and \code{NULL} is stored in the corresponding
#'    slot of the output list.
#'
#' @param years (num) An integer vector of years (or an object
#'        cohercible to it) containing the years of fars files
#'        of interest.
#' @param path (chr) path to the fars file
#'
#' @return A list of data frame, one for each year privided in
#'         input, with two columns: \code{MONTH}, and \code{YEAR} of the
#'         corresponding fars file. If the file does not exist
#'         \code{NULL} is stored in the corresponding slot of the list.
#' @export
#'
#' @examples
#' \dontrun{
#'     file_path <- system.file("tests", "testthat",
#'         package = "weektwo"
#'     )
#'
#'     fars_read_years(c(2012, 2013, 2014), path = file_path) # one warn
#'     fars_read_years(c(2013, 2014)) # no warning
#'     fars_read_years(c("2012", "2013", "2014")) # works with characters
#' }
fars_read_years <- function(years, path = ".") {

    years <- suppressWarnings(
        purrr::set_names(as.numeric(years), as.numeric(years))
    )

    assertive::assert_all_are_whole_numbers(years)

    purrr::map(years, function(year) {

        file_path <- file.path(path, make_filename(year))

        tryCatch(
            fars_read(file_path)[c("MONTH", "YEAR")],
            error = function(e) {
                warning("invalid year: ", year, call. = FALSE)
                return(NULL)
            }
        )
    })

}
