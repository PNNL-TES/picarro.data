
#' Assign sample numbers.
#'
#' @param raw_data A \code{data.frame} of data returned by \code{\link{process_directory}}.
#' @return The data with new \code{Sample_number} and \code{Elapsed_seconds} columns.
#' @import dplyr
#' @importFrom tidyr replace_na
#' @export
assign_sample_numbers <- function(raw_data) {
  stopifnot(!"Sample_number" %in% colnames(raw_data))
  stopifnot("MPVPosition" %in% colnames(raw_data))

  raw_data %>%
    # Whenever the valve position changes, that's a new sample starting
    mutate(newsample = MPVPosition != lag(MPVPosition)) %>%
    replace_na(list(newsample = FALSE)) %>%
    mutate(Sample_number = cumsum(newsample)) %>%
    select(-newsample) %>%
    # Compute Elapsed_seconds for each sample
    group_by(Sample_number) %>%
    mutate(Elapsed_seconds = as.double(difftime(DATETIME, min(DATETIME), units = "secs")))
}


#' Clean the data: create \code{DATETIME} field, remove fractional and unwanted valves
#'
#' @param raw_data A \code{data.frame} of data returned by \code{\link{process_directory}}.
#' @param tz Timezone to use for timestamps, e.g. "America/New_York", character.
#' See \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones}.
#' @param remove_valves An optional vector of integer valve numbers to remove
#' from the data, e.g. ambient ports.
#' @return The cleaned data.
#' @export
clean_data <- function(raw_data, tz = "", remove_valves = c()) {
  if(tz == "") {
    warning("Time zone blank and so being set to UTC; is this correct?")
    tz <- "UTC"
  }

  if(length(remove_valves)) {
    message("Removing valves ", remove_valves)
  }

  raw_data %>%
    # Create DATETIME field and select columns we need
    mutate(DATETIME = as.POSIXct(paste(DATE, TIME),
                                 format = "%Y-%m-%d %H:%M:%S", tz = tz)) %>%
    select(DATETIME, ALARM_STATUS, MPVPosition, CH4_dry, CO2_dry, h2o_reported) %>%

    # Discard any fractional valve numbers
    filter(MPVPosition == floor(MPVPosition)) %>%
    # Remove unwanted valves
    filter(!MPVPosition %in% remove_valves)
}
