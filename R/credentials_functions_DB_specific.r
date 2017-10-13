# ======================================================== #
#   multi-function roxygen2 page using @rdname with NULL   #
# ======================================================== #

#' Read to / Write from credential files
#' 
#' Securely write sensitive info to disk and then read in and decrypt.
#' 
#' There are two sets of pairs of functions
#' use \code{writeCredentialsToFile()} to output to disk
#' use \code{readCredentialsFromFile()} to read in the credentials back to R
#' 
#' Similarly, there are a pair of functions with the 5 comonly-used parameters for database connections
#' use \code{writeDBCredentialsToFile()} and \code{readDBCredentialsFromFile()}
#' 
#' @param TBD
#'
#' @return 
#' for \code{writeCredentialsToFile} & \code{writeDBCredentialsToFile}: The file path where the encrypted values have been stored. 
#' 
#' for \code{readCredentialsFromFile} & \code{readDBCredentialsFromFile}: A list object of the decrypted values.
#' 
#' @name read_write_redentials_files
#' 
#' @examples
#'   "TBD"
NULL

#' @rdname read_write_redentials_files
#' @export
readDBCredentialsFromFile <- function(
    file_full_path     = "auto"
  , info.file_name     = ""
  , file_name          = getOption("rcreds.db.file_name", default=".db_credentials.creds")
  , folder             = getOption("rcreds.db.folder",    default="~/.rcreds/db_credential_files")
  , key                = readKeyFromFile()
) {

  args <- collectArgs(except=c("file_full_path", "info.file_name", "file_name"))
  do.call(readCredentialsFromFile, args)
}

#' @rdname read_write_redentials_files
#' @importFrom magrittr %<>%
#' @export
writeDBCredentialsToFile <- function(
    dbname            = "dev"
  , host               = "localhost"
  , port               = 5432
  , username           = "you_forgot_to_specify_username"
  , password           = "too_many_secrets"
  , file_full_path     = "auto"
  , info.file_name     = ""
  , file_name          = getOption("rcreds.db.file_name", default=".db_credentials.creds")
  , folder             = getOption("rcreds.db.folder",    default="~/.rcreds/db_credential_files")
  , zArchive_existing  = TRUE
  , overwrite_existing = FALSE
  , key                = readKeyFromFile()
  , ...
) {

  stopifnot(requireNamespace("digest"))
  stopifnot(requireNamespace("jsonlite"))

  ## Create 'file_full_path' if auto.  Otherwise, warn when any pieces were given explicitly.
  ## --------------------------------------------------------------------------------- ##
  if (isTRUE(file_full_path == "auto")) {
    file_full_path <- construct_rcreds_file_full_path(file_name=file_name, folder=folder, info.file_name=info.file_name)
  } else if (!missing(file_full_path)) {
      if ((!missing(file_name) || !missing(folder)) || !missing(info.file_name))
        warning("Parameters 'file_name', 'folder', and 'info.file_name' are ignored when 'file_full_path' is set explicitly")
  }
  ## --------------------------------------------------------------------------------- ##

  
  args <- collectArgs(except=c("file_full_path", "info.file_name", "file_name"))

  ## Ensure ordering of args
  first_args <- c("dbname", "host", "port", "username", "password")
  matched <- match(first_args, names(args)) %>% {.[!is.na(.)]}
  args %<>% {c(.[matched], .[-matched])}

  message("NAME OF args:")
  catnn(names(args))
  do.call(writeCredentialsToFile, args)
}

