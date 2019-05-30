
#' Assign sample numbers, and optionally remove some valves' data.
#'
#' @param raw_data A \code{data.frame} of data returned by \code{\link{process_directory}}.
#' @param remove_valves An optional vector of integer valve numbers to remove
#' from the data, e.g. ambient ports.
#' @return The (possibly filtered) data with a new \code{Sample_number} column.
#' @import dplyr
#' @importFrom tidyr replace_na
#' @export
assign_sample_numbers <- function(raw_data, remove_valves = c()) {
  stopifnot(!"Sample_number" %in% colnames(raw_data))
  stopifnot("MPVPosition" %in% colnames(raw_data))

  if(length(remove_valves)) {
    message("Removing valves ", remove_valves)
  }

  raw_data %>%
    # Whenever the valve position changes, that's a new sample starting
    mutate(newsample = MPVPosition != lag(MPVPosition)) %>%
    replace_na(list(newsample = FALSE)) %>%
    mutate(Sample_number = cumsum(newsample)) %>%
    select(-newsample) %>%
    # Remove unwanted valves
    filter(!MPVPosition %in% remove_valves)
  # group_by(samplenum) %>%
  # mutate(elapsed_seconds = as.double(difftime(DATETIME, min(DATETIME), units = "secs"))) ->
  # rawdata_samples
}
