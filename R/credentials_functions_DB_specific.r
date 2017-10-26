#' @rdname credentials_functions
#' @importFrom collectArgs collectArgs
#' @export
read_db_credentials_from_file <- function(
    file_full_path       = "..auto.."
  , info.file_name       = ""
  , file_name            = getOption("rcreds.db.file_name", default=".db_credentials.creds")
  , folder               = get_default_rcreds_folder(DB=TRUE)
  , key                  = read_key_from_file()
  , fail_if_cant_decrypt = TRUE
  , showWarnings         = TRUE
  , verbose              = getOption("verbose.rcreds", default=TRUE)
) {


  stopifnot(requireNamespace("collectArgs"))

  ## Create 'file_full_path' if auto.  Otherwise, warn when any pieces were given explicitly.
  ## --------------------------------------------------------------------------------- ##
  if (isTRUE(file_full_path == "..auto..")) {
    file_full_path <- construct_rcreds_file_full_path(file_name=file_name, folder=folder, info.file_name=info.file_name)
  } else if (!missing(file_full_path)) {
      if ((!missing(file_name) || !missing(folder)) || !missing(info.file_name))
        warning("Parameters 'file_name', 'folder', and 'info.file_name' are ignored when 'file_full_path' is set explicitly")
  }
  ## --------------------------------------------------------------------------------- ##

  args <- collectArgs::collectArgs(except=c("folder", "info.file_name", "file_name"))
  do.call(read_credentials_from_file, args)
}

#' @rdname credentials_functions
#' @importFrom magrittr %<>%
#' @importFrom collectArgs collectArgs
#' @export
write_db_credentials_to_file <- function(
    dbname             = "dev"
  , host               = "localhost"
  , port               = 5432
  , username           = "you_forgot_to_specify_username"
  , password           = "too_many_secrets"
  , file_full_path     = "..auto.."
  , info.file_name     = ""
  , file_name          = getOption("rcreds.db.file_name", default=".db_credentials.creds")
  , folder             = get_default_rcreds_folder(DB = TRUE)
  , allow_root_user    = FALSE
  , zArchive_existing  = TRUE
  , overwrite_existing = FALSE
  , key                = read_key_from_file()
  , ...
  , verbose            = getOption("verbose.rcreds", default=TRUE)  
) {

  .stop_if_root(allow_root_user=allow_root_user)

  stopifnot(requireNamespace("digest"))
  stopifnot(requireNamespace("jsonlite"))
  stopifnot(requireNamespace("collectArgs"))

  ## Create 'file_full_path' if auto.  Otherwise, warn when any pieces were given explicitly.
  ## --------------------------------------------------------------------------------- ##
  if (isTRUE(file_full_path == "..auto..")) {
    file_full_path <- construct_rcreds_file_full_path(file_name=file_name, folder=folder, info.file_name=info.file_name)
  } else if (!missing(file_full_path)) {
      if ((!missing(file_name) || !missing(folder)) || !missing(info.file_name))
        warning("Parameters 'file_name', 'folder', and 'info.file_name' are ignored when 'file_full_path' is set explicitly")
  }
  ## --------------------------------------------------------------------------------- ##

  
  args <- collectArgs::collectArgs(except=c("folder", "info.file_name", "file_name"))

  ## Ensure ordering of args
  first_args <- c("dbname", "host", "port", "username", "password")
  matched <- match(first_args, names(args)) %>% {.[!is.na(.)]}
  args %<>% {c(.[matched], .[-matched])}

  # message("NAME OF args:")
  # catnn(names(args))
  do.call(write_credentials_to_file, args)
}

