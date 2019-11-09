
#' Assign sample numbers and compute \code{Elapsed_seconds} for each sample.
#'
#' @param raw_data A \code{data.frame} of data returned by \code{\link{process_directory}}.
#' @return The data with new \code{Sample_number} and \code{Elapsed_seconds} columns.
#' @export
assign_sample_numbers <- function(raw_data) {
  if(is.null(raw_data)) return(NULL)

  stopifnot(!"Sample_number" %in% colnames(raw_data))
  stopifnot("MPVPosition" %in% colnames(raw_data))
  stopifnot("DATETIME" %in% colnames(raw_data))

  # Whenever the valve position changes, that's a new sample starting
  newsample <- raw_data$MPVPosition != c(NA, head(raw_data$MPVPosition, -1))
  newsample[is.na(newsample)] <- FALSE
  raw_data$Sample_number = cumsum(newsample)

  # Compute Elapsed_seconds for each sample
  raw_data$Elapsed_seconds <- NA_real_
  for(sn in unique(raw_data$Sample_number)) {
    x <- sn == raw_data$Sample_number
    dt <- raw_data$DATETIME[x]
    raw_data$Elapsed_seconds[x] <- as.double(difftime(dt, min(dt), units = "secs"))
  }
  raw_data
}


#' Clean the data: create \code{DATETIME} field, remove fractional and unwanted valves
#'
#' @param raw_data A \code{data.frame} of data returned by \code{\link{process_directory}}.
#' @param tz Timezone of Picarro timestamps, e.g. "America/New_York", character.
#' See \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones}.
#' @param remove_valves An optional vector of integer valve numbers to remove
#' from the data, e.g. ambient ports.
#' @return The cleaned data.
#' @export
clean_data <- function(raw_data, tz = "", remove_valves = c()) {
  if(is.null(raw_data)) return(NULL)

  if(tz == "") {
    warning("Time zone blank and so being set to UTC; is this correct?")
    tz <- "UTC"
  }

  if(length(remove_valves)) {
    message("Removing valves ", remove_valves)
  }

  # Create DATETIME field and select columns we need
  raw_data$DATETIME = as.POSIXct(paste(raw_data$DATE, raw_data$TIME),
                                 format = "%Y-%m-%d %H:%M:%S",
                                 tz = tz)
  raw_data <- raw_data[c("DATETIME", "ALARM_STATUS", "MPVPosition",
                         "CH4_dry", "CO2_dry", "h2o_reported")]

  DATETIME <- MPVPosition <- NULL  # silence package check notes

  subset(raw_data,
         # discard any fractional valve numbers,
         MPVPosition == floor(MPVPosition) &
         # invalid timestamps,
         !is.na(DATETIME) &
         # and unwanted valves
         !MPVPosition %in% remove_valves)
}
