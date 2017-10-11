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

.confirm_is_string_of_length1 <- function(string, nm_for_err_msg, empty_string_ok=FALSE) {
  if (missing(nm_for_err_msg)) {
    nm_for_err_msg <- as.character(substitute(string))
  }

  if (length(string) != 1)
    stop("Invalid input for '", nm_for_err_msg, "'.  It should be a non-empty string of length 1 -- it has length ", length(string))
  if (!is.character(string))
    stop("Invalid input for '", nm_for_err_msg, "'.  It should be a non-empty string of length 1 -- it is an object of class \"", class(string)[[1L]], "\"")
  if (!empty_string_ok && !nzchar(string))
    stop("Invalid input for '", nm_for_err_msg, "'.  It should be a non-empty string of length 1 -- it is an empty string.")

  return(invisible(TRUE))
}

if (!requireNamespace("rsugeneral")) {
  
#' @example  some_function(x, param1, param2, etc, ...) {
#' @example    ARGS <- collectArgs(except="x")
#' @example    return(lapply(x, function(x_i) do.call(fwp, c(ARGS, x=x_i))))
#' @example  }

  collectArgs <- function(except=c(), incl.dots=TRUE, all.names=TRUE, envir=parent.frame()) {
  ## GENERAL USAGE:
  #  if (is.list(x)) {
  #    ARGS <- collectArgs(except="x")
  #    return(lapply(x, function(x_i) do.call(fwp, c(ARGS, x=x_i))))
  #  }

    force(envir)

    args <- ls(envir=envir, all.names=all.names) %>% setdiff("...")
    args <- setdiff(args, except)
    data.table::setattr(args, "names", args)
    ret <- lapply(args, function(x) get(x, envir=envir) )
    
    if (incl.dots && exists("...", envir=envir))
        ret <- c(ret, eval(quote(list(...)), envir=envir))
    
    return(ret)
  }
}