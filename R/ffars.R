

#' Read Fatality Analysis Reporting System (FARS) data
#' Read FARS data in comma-separated files (compression optional) into \code{tbl_df}
#' Fails if filename not available - use \link{make_filename}  for canonical file nameing
#'
#' @param filename - the filename, a bzipped .csv file supplied with the assignment
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @return The data as a \code{tbl_df}
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}



#' Create canonical filenmae
#'
#' @param year - the year as four digits. Data set includes only 2013-2015
#'
#' @return the filename styled including the ending for bz2-compressed csv
#' @examples make_filename(2013)
#' @note Internal function, will blindly assume names without any checking of existence of file
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

