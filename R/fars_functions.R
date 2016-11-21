#' Read FARS file
#'
#' This is a simple function that reads in FARS data files. If the
#' file doesn't exist an error message will be returned:
#' Error in fars_read("test.csv") : file 'test.csv' does not exist
#'
#' @param filename the name along with the file path for the data
#'
#' @return This function returns a data table of the class tbl_df
#'
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Make filename
#'
#' This is a simple function that creates a bz2 filename using the year
#' input and the "accident". The function will attempt to convert the input
#' to a numeric, returning NA where this is not possible.
#'
#' @param year the year that you wish to include in your filename.
#'
#' @return This function returns a string filename including the year
#' input converted in a numeric value.
#'
#' @examples
#' make_filename("2015")
#' make_filename(2015)
#' make_filename(year = 2015)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS Years
#'
#' This is a simple function that creates filenames for each of the values
#' in the input and then forms a list of tables from the files which match the list
#' of filenames.
#'
#' @param years A character vector of years which will be cycled through
#' to form the list of data sets
#'
#' @return This function returns a list of tbl_df objects, each containing
#' data for the years given in the input. An error will be returned for invalid
#' years.
#'
#'
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

#' FARS Summarize Years
#'
#' This is a simple function that summarises the list of tables that result
#' from running the function fars_read_years
#'
#' @param years A character vector of years which will be cycled through
#' to form the summarized data set
#'
#' @return This function returns a phrase to print, with or without an
#'    exclamation point added. As a side effect, this function also prints out
#'    the phrase.
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' FARS Map State
#'
#' This is a simple function that filters the data from a specific year
#' by a state. The filtered data is then plotted by to show the accident
#' data graphically by longitude and lattitude values.
#'
#' @param state.num A numeric value for the STATE in the data frame
#' @param year A numeric value for the year in the filename
#'
#' @return This function returns a map with the accidents plotted on it
#' by longitude and lattitude.
#'
#'
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
