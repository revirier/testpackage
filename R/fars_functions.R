#' @title
#' Load a FARS data file in R
#'
#' @description
#' This simple function read a FARS datafile located in the default directory
#'
#' @param filename The datafile filename
#'
#' @return A data frame. As a side effect, this function also prints out the first lines of the data frame loaded
#'
#' @details An error is raised if the filename doesnt exist
#' @details Uses the dplyr and readr packages
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
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

#' @title
#' Create the FARS filename for a year
#'
#' @description
#' This simple function create the filename of a FARS file for the year passed
#'
#' @param year An year in numeric format
#'
#' @return A string with the FARS filename
#'
#' @examples
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("../data/accident_%d.csv.bz2", year)
}

#' @title
#' Filter the FARS data by year
#'
#' @description
#' Filter the FARS data for the years passed as a list and return a dataframe with two columns: MONTH and YEAR
#'
#' @param years A list with the years to filter
#'
#' @return A data frame with two columns YEAR and MONTH. As a side effect, this function also prints out the first lines of the data frame loaded
#'
#' @details An error is raised if doesnt exist data for the year passed
#' @details Uses the dplyr and magrittr packages
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @examples
#' fars_read_years(c(2013,2014,2015))
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

#' @title
#' Summarize the rows of FARS data by year and month
#'
#' @description
#' Summarize the FARS data by month for the years passed as a list and return a dataframe with one row by month and one column for each year
#'
#' @param years A list with the years to filter
#'
#' @return A data frame with the data summarize by month
#'
#' @details Uses the dplyr, tidyr and magrittr packages
#'
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#'
#'
#' @examples
#' fars_summarize_years(c(2013,2014,2015))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' @title
#' Show a plot a map with the year's accidents in a state
#'
#' @description
#' this plot a map for the state provided showing the accidents of the year geolocalized
#'
#' @param state.num an integer with the state code
#' @param year An integer with the year
#'
#' @return A plot of the state with points on the accident
#'
#' @details An error is raised if the state code is not valid
#' @details An error is raised if doesnt exists accidentes data for the state plotted
#' @details Use the maps and graphics packages
#'
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' fars_map_state(42,2015)
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
