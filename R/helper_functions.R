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

.confirm_is_string_of_length1 <- function(string, nm_for_err_msg, empty_string_ok=FALSE, NAs_ok=FALSE) {
  if (missing(nm_for_err_msg)) {
    nm_for_err_msg <- as.character(substitute(string))
  }

  if (length(string) != 1)
    stop("Invalid input for '", nm_for_err_msg, "'.  It should be a non-empty string of length 1 -- it has length ", length(string))
  if (!is.character(string))
    stop("Invalid input for '", nm_for_err_msg, "'.  It should be a non-empty string of length 1 -- it is an object of class \"", class(string)[[1L]], "\"")
  if (!empty_string_ok && !nzchar(string))
    stop("Invalid input for '", nm_for_err_msg, "'.  It should be a non-empty string of length 1 -- it is an empty string.")
  if (!NAs_ok && is.na(string))
    stop("Invalid input for '", nm_for_err_msg, "'.  It should be a non-empty string of length 1 -- it is NA.")

  return(invisible(TRUE))
}

.confirm_is_logical_of_length1 <- function(x, nm_for_err_msg, NAs_ok=FALSE) {
  if (missing(nm_for_err_msg)) {
    nm_for_err_msg <- as.character(substitute(x))
  }

  it_should_be <- ifelse(NAs_ok, "'.  It should be a logical of length 1 -- it "
                               , "'.  It should be a single TRUE/FALSE value -- it ")

  if (length(x) != 1)
    stop("Invalid input for '", nm_for_err_msg, it_should_be, "has length ", length(x))
  if (!is.logical(x))
    stop("Invalid input for '", nm_for_err_msg, it_should_be, "is an object of class \"", class(x)[[1L]], "\"")
  if (!NAs_ok && is.na(x))
    stop("Invalid input for '", nm_for_err_msg, it_should_be, "is NA")

  return(invisible(TRUE))
}

.stop_if_root <- function(allow_root_user=FALSE) {
  if (!allow_root_user) {
    root_user <- "root"  ## not sure what this is on Windows machines
    if (!allow_root_user && Sys.getenv("USER") == root_user){
      stop("The current user is '", root_user, "'\nTo proceed the 'allow_root_user' must be set to TRUE\n\nNOTE: If the files are created under a root user, it is possible that\n       other users will NOT be able to use those files\n       (wihtout subsequent permission modifications).")
    }
  }
  return(invisible(NULL))
}

.is_any_element_NULL <- function(L, recursive=TRUE) {
  is.null(L) || (is.list(L) && any(vapply(L, FUN=.is_any_element_NULL, FUN.VALUE=logical(1L),  recursive=recursive)))
}

