

#' Read a single Picarro output file.
#'
#' @param filename Fully qualified filename to read.
#' @return A \code{data.frame} of the data.
#' @note This can read plain-text or compressed (via \code{zip} or \code{gz}) files.
#' @importFrom tibble as_tibble
#' @importFrom utils read.table unzip
#' @export
read_picarro_file <- function(filename) {
  message("Reading ", filename)
  stopifnot(file.exists(filename))

  f <- filename
  if(grepl(".gz$", filename)) {
    f <- gzfile(filename)
  } else if(grepl(".zip$", filename)) {
    f <- unzip(filename,
               files = basename(gsub("\\.zip$", "", filename)),
               exdir = tempdir())
  }

  as_tibble(read.table(f, header = TRUE, stringsAsFactors = FALSE, row.names = NULL))
}


#' Scan a directory and read all Picarro files in it.
#'
#' @param path Folder to process, character.
#' @param recursive Scan (via \code{\link{list.files}}) subfolders? Logical.
#' @return A \code{data.frame} of the data.
#' @import dplyr
#' @note This can read plain-text or compressed (via \code{zip} or \code{gz}) files.
#' @export
process_directory <- function(path, recursive = TRUE) {

  filelist <- list.files(path = path,
                         pattern = "dat$|dat.gz$|dat.zip$",
                         recursive = recursive,
                         full.names = TRUE)
  filedata <- list()
  message("Found ", length(filelist), " files")
  for(f in filelist) {
    filedata[[f]] <- read_picarro_file(f)
  }

  bind_rows(filedata, .id = "Filename")
}
