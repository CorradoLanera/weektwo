#' Read fars files
#'
#' Read a fars file returning the imported
#' \code{\link[dplyr]{data_frame}}.
#'
#' An error is returned if the path is not valid.
#'
#' @details Fars file has to be stored in a \code{CSV} file format.
#'   It can be possibly compressed (see \code{\link{file}}).
#'
#' @param filename (chr) Complete path to the fars file to read.
#'
#' @return A \code{\link[dplyr]{data_frame}}.
#' @export
#'
#' @examples
#' \dontrun{
#'    file_path <- system.file("tests", "testthat", package = "weektwo",
#'        "accident_2013.csv.bz2"
#'    )
#'    fars_read(file_path)
#' }
fars_read <- function(filename) {

    if (!file.exists(filename)) {
        stop("file '", filename, "' does not exist in", getwd())
    }

    suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
}
