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
#' @param path (chr) path to the fars file
#'
#' @return \code{NULL} (with a message) if the STATE represented by the
#'         \code{state.num} provided has no records in fars for the year
#'         selected. Otherwhise, a plot with the geographical point for
#'         its fars is produced and nothing will be returned.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#'     file_path <- system.file("tests", "testthat",
#'         package = "weektwo"
#'     )
#'
#'     fars_map_state(1, 2013, path = file_path)
#'     fars_map_state(1, 2012, path = file_path) # error
#'     fars_map_state(2, 2015, path = file_path) # nothing to drawn
#' }
fars_map_state <- function(state.num, year, path = ".") {

    file_path <- file.path(path, make_filename(year))
    data <- fars_read(file_path)
    state.num <- as.integer(state.num)

    if (!(state.num %in% unique(data[["STATE"]])))
        stop("invalid STATE number: ", state.num)
    data.sub <- data[data[["STATE"]] == state.num, , drop = FALSE]
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
