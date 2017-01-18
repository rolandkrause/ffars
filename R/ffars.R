

#' Read Fatality Analysis Reporting System (FARS) data
#' Read FARS data in comma-separated files (compression optional) into \code{tbl_df}
#' Fails if filename not available - use \link{make_filename}  for canonical file nameing
#'
#' @param filename - the filename, a bzipped .csv file supplied with the assignment
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples
#' \dontrun{fars_read("accident_2013.csv.bz2")}
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

#' Read years
#'
#' Read FARS data into memory and select month and years
# @inheritParams make_filename # actually does not work as advertised in the book
#' @importFrom dplyr mutate select
#' @param years vector of four digit years
#' @return A \code{tbl_df} with month and years as columns
#' @examples
#' \dontrun{fars_read_years(c(2013, 2014))}
#' @note Uses \code{\link{make_filename}} internally, warns on invalid year
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize years by month
#' @param years vector of years
#' @return tbl_df with MONTH and the respective years as columns
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#' @examples
#' \dontrun{fars_summarize_years(c(2013, 2014))}
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Map accidents by state and year
#'
#' Plots the location of accidents on maps of US states
#'
#' @param state.num The numerical ID of a US state
#' @param year the year
#' @importFrom dplyr filter
#' @importFrom maps map
#' @return A map of the state and location of accident - an image
#'
#' @examples
#' \dontrun{fars_map_state(48, 2013)}
#'
#' @note Requires dplyr and maps package. To be called at package level via 'importFrom'
#' statements
#' @note Errors on state numbers not included in the data package
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
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




#' Create canonical filenmae
#'
#' @param year - the year as four digits. Data set includes only 2013-2015
#'
#' @return the filename styled including the ending for bz2-compressed csv
#' @examples make_filename(2013)
#' @note Internal function, will blindly assume names without any checking of existence of file
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

