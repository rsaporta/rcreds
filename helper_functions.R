## Do not export
## More robust functions exist in rsuworkspace package
## namely  rsuworkspace::zArchive()  and rsuworkspace::zArchive_if_flagged()
.zArchive_existing_file <- function(file_full_path, verbose=getOption("verbose.rcreds", default=TRUE)) {
  stamp <- format(Sys.time(), format=".zarchived_%Y%m%d_%H%M%S")
  file_zArchived <- file_full_path %>% {file.path(dirname(.), "zArchived", basename(.))} %>% paste0(stamp)

  dir.create(path=dirname(file_zArchived), showWarnings = FALSE, mode="0775")
  ret <- try(file.rename(from=file_full_path, to=file_zArchived), silent=TRUE)

  ## CHECK FOR ERRORS AND THAT FILE WAS MOVED
  if (inherits(ret, "try-error"))
    stop("attempting to archive the file with function file.rename() failed with the following error:\n     ", as.character(ret))
  if (file.exists(file_full_path))
    stop("Attempted to zArchive the file '", file_full_path, "', but failed")

  if (verbose)
    message("existing file moved to:  '", file_zArchived, "'")
  return(file_zArchived)
}

.confirm_is_string_of_length1 <- function(string, nm_for_err_msg) {
  if (missing(nm_for_err_msg)) {
    nm_for_err_msg <- as.character(substitute(string))
  }

  if (length(string) != 1)
    stop("Invalid input for '", nm_for_err_msg, "'.  It should be a non-empty string of length 1 -- it has length ", length(string))
  if (!is.character(string))
    stop("Invalid input for '", nm_for_err_msg, "'.  It should be a non-empty string of length 1 -- it is an object of class \"", class(string)[[1L]], "\"")
  if (!nzchar(string))
    stop("Invalid input for '", nm_for_err_msg, "'.  It should be a non-empty string of length 1 -- it is an empty string.")

  return(invisible(TRUE))
}