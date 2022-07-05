#' Import data
#'
#' This is a function to import data in CSV format given a file name.
#'
#' @param filename A character string of the file name to be imported
#'
#' @return A tibble of the data requested
#'
#' @note An error will occur if the file does not exist in the working directory.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#'   d <- fars_read("accident_2015.csv.bz2")
#' }
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make file name
#'
#' This function creates an "accident_year.csv.bz2" file name given the year entered by the user.
#'
#' @param year The year to be attached to the file name.
#'
#' @return This function returns a file name with the input year.
#'
#' @examples
#' fname <- make_filename(2015)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Import data of the years requested
#'
#' This function accepts a vector of years of the data requested and
#' creates a list of tibbles with the month and year columns based
#' on the input years.
#'
#' @param years A vector of years of the data requested
#'
#' @return A list of tibbles with the month and year columns requested
#'
#' @note An error will occur if the file does not exist in the working directory.
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr `%>%`
#'
#' @examples
#' \dontrun{
#'   d <- fars_read_years(c(2013,2014,2015))
#' }
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

#' Summarising data of the years requested
#'
#' This function accepts a vector of years of the data requested and
#' summaries the number of accidents by month and year.
#'
#' @param years A vector of years of the data requested
#'
#' @return A tibble summarising the number of accidents by month and year
#'
#' @note An error will occur if the file does not exist in the working directory.
#'
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#' @importFrom magrittr `%>%`
#'
#' @examples
#' \dontrun{
#'   d <- fars_summarize_years(c(2013,2014,2015))
#' }
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = dplyr::n()) %>%
                tidyr::spread(year, n)
}

#' Import data of the years requested
#'
#' This function accepts a vector of years of the data requested and
#' summaries the number of accidents by month and year.
#'
#' @param state.num An integer indicating the state to plot
#' @param year The year to plot
#'
#' @return A plot of the state showing the accidents
#'
#' @note An error will occur if the data does not contain the state requested.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#'   fars_map_state(1,2015)
#' }
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
