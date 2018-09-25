#' Read fars
#'
#' Read a fars file  returning the imported \code{data_frame}. An error is
#' returned if the path is not valid.
#'
#' @details Fars file has to be stored in a \code{CSV} file format.
#'   It can be possibly compressed (see \code{\link{file}}).
#'
#' @param filename (chr) Complete path to the fars file to read.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @return A \code{data_frame}.
#' @export
#'
#' @examples
#' \dontrun{
#'     library(weektwo)
#'     sample_data <- system.file("sample-data/accident_2013.csv.bz2",
#'         package = "weektwo"
#'     )
#'
#'     fars_read(sample_data)
#' }
fars_read <- function(filename) {
    if (!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}

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
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}


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
#'
#' @importFrom dplyr mutate select
#'
#' @return A list of \code{data_frame}, one for each year privided in
#'         input, with two columns: \code{MONTH}, and \code{year} of the
#'         corresponding fars file. If the file does not exist
#'         \code{NULL} is stored in the corresponding slot of the list.
#' @export
#'
#' @examples
#' \dontrun{
#'     fars_read_years(c(2012, 2013, 2014))                # one warning
#'     fars_read_years(c(2013, 2014))                       # no warning
#'     fars_read_years(c("2012", "2013", "2014"))# works with characters
#' }
fars_read_years <- function(years) {
    lapply(years, function(year) {
        file <- make_filename(year)
        tryCatch({
            dat <- fars_read(file)
            dplyr::mutate(dat, year = year)[, c("MONTH", "year")]
        }, error = function(e) {
            warning("invalid year: ", year)
            return(NULL)
        })
    })
}

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
#' @return A \code{data_frame} with a number of column equals to the
#'         number of (valid) years passed as input (plus one for the
#'         months) and one row each month included in the corresponding
#'         fars files. The \code{data_frame} is filled with the number
#'         of records included for the corresponding year and month.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
#'
#' @examples
#' \dontrun{
#'     fars_summarize_years(c(2013, 2014))
#'     fars_summarize_years(c(2012, 2013, 2014))
#' }
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}

#' Map accidents
#'
#' Plot the point map for the fars record of the STATE corresponding to
#' the \code{state.num} and the \code{year} provided
#'
#' @details if no accident happen in the state and period selected
#'          a message will be throw and \code{NULL} is returned.
#'
#'          If a plot is returned, it will be drawn only for longitude
#'          not greater than 900 and latidude not grater than 90.
#'
#' @inheritParams make_filename
#' @param state.num (num) An integer (or something cohercible to it)
#'        representing a STATE number into the fars data.
#'
#' @return \code{NULL} (with a message) if the STATE represented by the
#'         \code{state.num} provided has no records in fars for the year
#'         selected. Otherwhise, a plot with the geographical point for
#'         its fars is produced and nothing will be returned.
#'
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
#'
#' @examples
#' \dontrun{
#'     fars_map_state(1, 2013)
#'     fars_map_state(1, 2012) # error
#'     fars_map_state(2, 2015) # nothing to drawn
#' }
fars_map_state <- function(state.num, year) {
    filename <- make_filename(year)
    data <- fars_read(filename)
    state.num <- as.integer(state.num)

    if (!(state.num %in% unique(data[["STATE"]])))
        stop("invalid STATE number: ", state.num)
    data.sub <- dplyr::filter(data, STATE == state.num)
    if (nrow(data.sub) == 0L) {
        message("no accidents to plot")
        return(invisible(NULL))
    }
    is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
    is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
    with(data.sub, {
        maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
            xlim = range(LONGITUD, na.rm = TRUE))
        graphics::points(LONGITUD, LATITUDE, pch = 46)
    })
}
