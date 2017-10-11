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

  
#' collectArgs
#'
#' Colects arguments from an envrionment into a single list. Generally used with \code{do.call()}
#'
#' @param except A vector of string values. Objects to \emph{NOT} include in the collection
#' @param incl.dots A single logical value. Should the \code{...} be collected as well?  Default is \code{TRUE}.
#'                  \emph{NOTE: Has no effect in functions without dots argument}
#' @param all.names A single logical value. Passed to \code{ls()}. When \code{FALSE}, then objects whose name begins with a '.' are omitted from the collection
#' @param envir     An \code{environment} object. Passed to \code{ls()}. The environment from which to collect the objects. Defaults to \code{parent.frame}
#'
#' @return A list of all of the objects in \code{envir} (\emph{less any objects excluded via the parameters}), whose names in the list are the names of object in \code{envir}.
#'
#' @examples
#'  some_function <- function(x, param1, param2, etc, ...) {
#'    ARGS <- collectArgs(except="x")
#'    return(
#'            lapply(x, function(x_i) 
#'               do.call(some_function, c(ARGS, x=x_i))
#'            )
#'          )
#'  }
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
if (!requireNamespace("rsugeneral")) {
}