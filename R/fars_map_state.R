#'Plot the location of accidents on US maps
#'
#' This function load accident data from
#' the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System,
#' into R for a specified year and state numbers,
#' validate the parameters and plot the location of accidents on a US map.
#'
#' @param state.num Integers or numerical values for a US State numbers included in the analysis
#' @param year An integer or numerical value for the year of the analysis
#'
#' @inheritParams make_filename
#' @inheritParams fars_read
#'
#' @return a US map woith the location of accidents for the year specified in the parameters
#' Returns \code{invalid STATE number} if the state number(s) can not be found in the
#' file or directory
#' Returns \code{no accidents to plot} if there are no accidents in the chosen file
#' for the analysis
#'
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(c(1:56),2014)
#' }
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if (!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~STATE == state.num)
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
