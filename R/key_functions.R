# ========================================================== #
#    key_functions.R                                         #
# ========================================================== #
#
#' Key Functions
#'
#' ONE-LINER WHAT DO THESE GROUP OF FUNCS DO? (or the name of the main function)
#'
#' DETAILED DESCRIPTION of what these functions do
#'
#' @name key_functions
#'
#X## -------------------------------  PARAMS  ------------------------------- ##
#' @param bytes Number of bytes used for the key.
#'              Values should normally be one of \code{c(16, 24, 32)}
#'
#'              Defaults to: 32
#'
#' @param depth Bit depth for key. 
#'
#'              Defaults to: 8
#'
#' @param seed An integer passed to \code{set.seed()} 
#'             Generating the key involves random number generation. Setting the seed will make the key determenistic. 
#'
#'             Defaults to: NULL
#'
#' @param file_full_path The full path to the creds (or key) file, where it should be read from or written to. 
#'     if \code{"..auto.."} then will be constructed from \code{folder}, \code{file_name}, and \code{info.file_name}
#'     
#'     NOTE: When \code{file_full_path} is set explicitly, then \code{folder}, \code{file_name}, and \code{info.file_name} are ignored.
#'
#'                       Defaults to: file.path(folder, file_name)
#'
#' @param file_name name of the file where the key will be written to or read from.  Should be a string of length 1
#'
#'                  Defaults to: getOption(\\"rcreds.key.file_name\\", default = \\".crypt_key.rds\\")
#'
#' @param folder folder where the credentials will be written to or read from.
#'
#'               Defaults to: get_default_rcreds_key_folder()
#'
#' @param create_if_not_exist A TRUE/FALSE flag. for \code{read_key_from_file}: If the given file does not exist, 
#'                    should a key be created and stored at that location?
#'
#'                    Defaults to: TRUE
#'
#' @param key A key object of class \code{"key_rcreds"} to be used for encrypting / decrypting. Passed to \code{digest::AES}.  
#' 
#'                 Alternatively, a full file path to a key stored on disk can be given which will be read to disk.
#'
#' @param allow_root_user A TRUE/FALSE flag.  If FALSE and user is root, then writing and saving functions will fail
#'                     This is a safety to make sure the user understands they are operating under root.
#'
#'                          Defaults to: FALSE
#'
#' @param zArchive_existing A TRUE/FALSE flag.  If \code{file_full_path} already exist, should it be moved to a zArchive folder?
#'
#'                          Defaults to: TRUE
#'
#' @param overwrite_existing A TRUE/FALSE flag.  If \code{file_full_path} already exist, should it be overwritten? This is only considered when \code{zArchive_existing} is FALSE
#'
#'                           Defaults to: FALSE
#'
#' @param showWarnings A TRUE/FALSE flag.  If FALSE, warnings will be silenced
#'
#'                           Defaults to: TRUE
#'
#' @param verbose A TRUE/FALSE flag.
#'
#'                Defaults to: getOption(\\"verbose.rcreds\\", default = TRUE)

#X## ------------------------------------------------------------------------ ##
#'
#' @return
#'
#'     for \code{create_key} and \code{read_key_from_file}:  An object of class key_rcreds
#'
#'     for \code{save_key} and \code{show_default_rcreds_key_file}:  A full file path. In the case of \code{save_key}, this is where the key has been written to.
#'
#'     for \code{is.key_rcreds}:  A TRUE/FALSE value indidcating if the input is of class \code{"key_rcreds"}
#'
#' @examples
#' 
#'  \dontrun{
#'    ### ---------------------------------------------- ###
#'    ## Default Folders need to be set. This shold be in an .Rprofile file
#'    ### ---------------------------------------------- ###
#'    ## generally use:  set_default_rcreds_ALL(parent_folder = "~/.rcreds/")
#'    set_default_rcreds_ALL(parent_folder = file.path(tempdir(), ".rcreds/"), 
#'                           create_if_not_exist = TRUE)
#'    ### ---------------------------------------------- ###
#'
#'    library(rcreds)
#' 
#'    key <- create_key()
#' 
#'    file_creds <- write_credentials_to_file(username="cosmo", password="too many secrets", key=key)
#'    file_key   <- save_key(folder="different/key/location")
#' 
#'    ### IN ANOTHER FILE
#'    key <- read_key_from_file(folder="different/key/location")
#'    creds <- read_credentials_from_file(key=key, fail_if_cant_decrypt=FALSE, showWarnings=FALSE)
#'    ## normally, leave the above flags as their default TRUE. Using FALSE for this example only.
#'  }
#'
NULL



#' @rdname key_functions
#' @importFrom magrittr %>%
#' @export
show_default_rcreds_key_file <- function() {
  file_name          = getOption("rcreds.key.file_name", default=".crypt_key.rds")
  folder             = get_default_rcreds_key_folder()
  file_full_path     = file.path(folder, file_name)

  return(file_full_path)
}


## Encryption functions
#' @rdname key_functions
#' @importFrom magrittr %>%
#' @export
create_key <- function(bytes=32, depth=8, seed=NULL, showWarnings=TRUE, verbose=getOption("verbose.rcreds", default=TRUE)) {
  ## Don't set seed unless given explicitly
  if (!is.null(seed))
    set.seed(seed)

  if (!(bytes %in% c(16, 24, 32)) && showWarnings)
    warning("bytes should generally be either 16, 24, 32")

  if (isTRUE(verbose))
    message(sprintf("Creating new key with bytes = %i  and  depth = %i.  seed was %s.", bytes, depth, ifelse(is.null(seed), "not set", seed)))

  ret <- seq(from=0, to=2^depth-1) %>%
          sample(size=bytes, replace=TRUE) %>%
          as.raw()

  class(ret) <- c("key_rcreds", class(ret))

  return(ret)
}

#' @rdname key_functions
#' @importFrom magrittr %>%
#' @export
is.key_rcreds <- function(key) {
  ## TODO:  warn when inhertis is true but length is false
  (length(key) > 1) && (inherits(x=key, what="key_rcreds"))
}

use_key <- function(key) {
  if (is.key_rcreds(key))
    return(key)

  if (!length(key))
    stop("'key' parameter has length 0", call.=FALSE)

  if (length(key) > 1)
    stop("'key' parameter should have length 1. (It should be a key object or the file path of a saved key).\n  It has length ", length(key))

  if (is.character(key) && nzchar(key)) {
    if (!file.exists(key))
      stop("'key' parameter is a string but it is not a known file. It has value: '", key, "'", call.=FALSE)
    file <- key ## save it for messaging
    key <- try(read_key_from_file(file_full_path=file, create_if_not_exist=FALSE), silent=FALSE)
    if (!is.key_rcreds(key))
      stop("the 'key' parameter was a file, but did not produce a key_rcreds object when read in.", call.=FALSE)
    return(key)
  }

  stop("'key' parameter should be a key object or the file path of a saved key.\n  Instead it is an object of class '", class(key)[[1L]], "'")
}

#' @rdname key_functions
#' @importFrom magrittr %>%
#' @export
save_key <- function(
    file_full_path     = file.path(folder, file_name)
  , file_name          = getOption("rcreds.key.file_name", default=".crypt_key.rds")
  , folder             = get_default_rcreds_key_folder()
  , key # no default value. If missing will be created using create_key()
  , bytes              = 32
  , depth              = 8
  , seed               = NULL
  , zArchive_existing  = TRUE
  , overwrite_existing = FALSE
  , showWarnings       = TRUE
  , allow_root_user    = FALSE
  , verbose            = getOption("verbose.rcreds", default=TRUE)
) {

  .stop_if_root(allow_root_user=allow_root_user)

  if (missing(key) && !missing(file_full_path) && is.key_rcreds(file_full_path))
    stop("'key' parameter is missing but 'file_full_path' is a key_rcreds object.\n\n  HINT: Do you have your parameters unnamed?\n        ie this happens when a user runs      `save_key(KeyObject)`")

  ## VALIDATE OR CREATE KEY
  if (missing(key)) {
      key <- create_key(bytes=bytes, depth=depth, seed=seed)
      if (!is.key_rcreds(key))
          stop("INTERNAL ERROR -- the object returned from create_key() is not a 'key_rcreds' object")
  } else if (!is.key_rcreds(key)) {
      stop("'key' must be a 'key_rcreds' object.  It has class '", class(key)[[1L]], "'")
  } else if (!missing(bytes) |  !missing(depth)  |  !missing(seed)  && showWarnings) {
      warning("arguments 'bytes', 'depth', and 'seed' are only used when 'key' is missing. Otherwise they are ignored.")
  }

  if (!file.exists(dirname(file_full_path)))
    dir.create(dirname(file_full_path), showWarnings=FALSE, recursive=TRUE, mode="0775")

  if (file.exists(file_full_path)) {
    if (!zArchive_existing && !overwrite_existing)
      stop("file '", file_full_path, "' already exists.\n  HINT:  set `zArchive_existing = TRUE` to move it or `overwrite_existing = TRUE` to overwrite it")
    if (zArchive_existing)
      .zArchive_existing_file(file_full_path)
  }

  saveRDS(key, file=file_full_path)

  if (verbose)
    message("key file written to \"", file_full_path, "\"")

  return(invisible(file_full_path))
}

#' @rdname key_functions
#' @importFrom magrittr %>%
#' @export
read_key_from_file <- function(
    file_full_path = file.path(folder, file_name)
  , file_name      = getOption("rcreds.key.file_name", default=".crypt_key.rds")
  , folder         = get_default_rcreds_key_folder()
  , create_if_not_exist = TRUE
  , showWarnings   = FALSE
  , verbose        = getOption("verbose.rcreds", default=TRUE)
) {
  if (!file.exists(file_full_path)) {
    if (create_if_not_exist)
      save_key(file_full_path = file_full_path)
    else
      stop("Key file '", file_full_path, "' does not exist")

    if (verbose)
      cat("Key File did not exist and was created in '", file_full_path, "'", sep="", "\n")
  }

  ret <- readRDS(file_full_path)

  if (!is.key_rcreds(ret) && showWarnings)
    warning("The key read in from file is not a 'key_rcreds' object.  It has class '", class(ret)[[1L]], "'")

  return(ret)
}

