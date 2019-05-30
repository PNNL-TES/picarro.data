

#' Read a single Picarro output file.
#'
#' @param filename Fully qualified filename to read.
#' @return A \code{data.frame} of the data.
#' @note This can read plain-text or compressed (via \code{zip} or \code{gz}) files.
#' @importFrom tibble as_tibble
#' @importFrom utils read.table
read_picarro_file <- function(filename) {
  message("Reading ", filename)
  stopifnot(file.exists(filename))

  f <- filename
  if(grepl(".gz$", filename)) {
    f <- gzfile(filename)
  } else if(grepl(".zip$", filename)) {
    f <- unz(filename)
  }

  as_tibble(read.table(f, header = TRUE, stringsAsFactors = FALSE, row.names = NULL))
}


#' Scan a directory and read all Picarro files in it.
#'
#' @param path Folder to process, character.
#' @param tz Timezone to use for timestamps, e.g. "America/New_York", character.
#' See \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones}.
#' @param recursive Scan (via \code{\link{list.files}}) subfolders? Logical.
#' @return A \code{data.frame} of the data.
#' @import dplyr
#' @note This can read plain-text or compressed (via \code{zip} or \code{gz}) files.
#' @export
process_directory <- function(path, tz = "", recursive = TRUE) {

  if(tz == "") {
    warning("Time zone blank and so being set to UTC; is this correct?")
    tz <- "UTC"
  }

  filelist <- list.files(path = path,
                         pattern = "dat$|dat.gz$|dat.zip$",
                         recursive = recursive,
                         full.names = TRUE)
  filedata <- list()
  message("Found ", length(filelist), " files")
  for(f in filelist) {
    read_picarro_file(f) %>%

      # Create DATETIME field and select columns we need
      mutate(DATETIME = as.POSIXct(paste(DATE, TIME),
                                   format = "%Y-%m-%d %H:%M:%S", tz = tz)) %>%
      select(DATETIME, ALARM_STATUS, MPVPosition, CH4_dry, CO2_dry, h2o_reported) %>%

      # Discard any fractional valve numbers
      filter(MPVPosition == floor(MPVPosition)) ->
      filedata[[f]]
  }

  bind_rows(filedata, .id = "Filename")
}
