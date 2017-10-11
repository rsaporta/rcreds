#' @export
show_default_rcreds_key_file <- function() {
  file_name          = getOption("rcreds.key.file_name", default=".crypt_key.rds")
  folder             = getOption("rcreds.key.folder",    default="~/.rcreds/key_files")
  file_full_path     = file.path(folder, file_name)

  return(file_full_path)
}


## Encryption functions
#' @export
createKey <- function(bytes=32, depth=8, seed=NULL, verbose=getOption("verbose.rcreds", default=TRUE)) {
  ## Don't set seed unless given explicitly
  if (!is.null(seed))
    set.seed(seed)

  if (!(bytes %in% c(16, 24, 32)))
    warning("bytes should generally be either 16, 24, 32")

  if (isTRUE(verbose))
    message(sprintf("Creating new key with bytes = %i  and  depth = %i.  seed was %s.", bytes, depth, ifelse(is.null(seed), "not set", seed)))

  seq(from=0, to=2^depth-1) %>%
    sample(size=bytes, replace=TRUE) %>%
    as.raw()
}


#' @export
saveKey <- function(
    file_full_path     = file.path(folder, file_name)
  , file_name          = getOption("rcreds.key.file_name", default=".crypt_key.rds")
  , folder             = getOption("rcreds.key.folder",    default="~/.rcreds/key_files")
  , key # no default value. If missing will be created using createKey()
  , bytes              = 32
  , depth              = 8
  , seed               = NULL
  , zArchive_existing  = TRUE
  , overwrite_existing = FALSE
) {

  if (missing(key))
    key <- createKey(bytes=bytes, depth=depth, seed=seed)
  else if (!missing(bytes) |  !missing(depth)  |  !missing(seed))
      warning("arguments 'bytes', 'depth', and 'seed' are only used when 'key' is missing. Otherwise they are ignored.")

  if (!file.exists(dirname(file_full_path)))
    dir.create(dirname(file_full_path), showWarnings=FALSE, recursive=TRUE, mode="0775")

  if (file.exists(file_full_path)) {
    if (!zArchive_existing && !overwrite_existing)
      stop("file '", file_full_path, "' already exists.\n  HINT:  set `zArchive_existing = TRUE` to move it or `overwrite_existing = TRUE` to overwrite it")
    if (zArchive_existing)
      .zArchive_existing_file(file_full_path)
  }

  saveRDS(key, file=file_full_path)

  return(invisible(key))
}

#' @export
readKeyFromFile <- function(
    file_full_path = file.path(folder, file_name)
  , file_name      = getOption("rcreds.key.file_name", default=".crypt_key.rds")
  , folder         = getOption("rcreds.key.folder",    default="~/.rcreds/key_files")
  , dont_create    = FALSE
  , verbose        = getOption("verbose.rcreds", default=TRUE)
) {
  if (!file.exists(file_full_path)) {
    if (verbose)
      message("Key File does not exist.  Will create one")
    saveKey(file_full_path = file_full_path)
  }

  readRDS(file_full_path)
}

