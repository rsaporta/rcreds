##  Adopted from
##      https://github.com/sdoyen/r_password_crypt/blob/master/crypt.R

## TODO: 
#   (1)
#      When reading key from file, check if it is a JSON, if so, parse it. 
#      If not, then read it with the key
#   (2)
#      Add a folder parameter to allow for different location.
#      Set this as an option
#      Do not use as.path

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
    sample(size=32, replace=TRUE) %>%
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


#' @export
writeCredentialsToFile <- function(
    ...
  , file_full_path     = file.path(folder, file_name)
  , file_name          = getOption("rcreds.file_name", default=".credentials.creds")
  , folder             = getOption("rcreds.folder",    default="~/.rcreds/credential_files")
  , zArchive_existing  = TRUE
  , overwrite_existing = FALSE
  , key                = readKeyFromFile()
) {
  stopifnot(requireNamespace("digest"))
  stopifnot(requireNamespace("jsonlite"))

  if ((!missing(file_name) || !missing(folder))  &&  !missing(file_full_path))
    warning("Parameters 'file_name' and 'folder' are ignored when 'file_full_path' is set explicitly")

  if (!file.exists(dirname(file_full_path)))
    dir.create(dirname(file_full_path), showWarnings=FALSE, recursive=TRUE, mode="0775")

  if (file.exists(file_full_path)) {
    if (!zArchive_existing && !overwrite_existing)
      stop("file '", file_full_path, "' already exists.\n  HINT:  set `zArchive_existing = TRUE` to move it or `overwrite_existing = TRUE` to overwrite it")
    if (zArchive_existing)
      .zArchive_existing_file(file_full_path)
  }



  ## ---------------------------------------------------------------------------- ##
  ## THIS SECTION SIMPLY TAKES THE dots AND CONVERTS THEM TO A LIST WITH NAMES
  ## IF THE DOTS WERE A SINGLE LIST, THEN USE THAT, UNCHANGED
  ## OTHERWISE, PUT THE DOTS INTO A LIST.
  ## THOSE DOTS THAT WERE EXPLICITLY GIVEN A NAME, LEAVE AS IS;
  ## THOSE THAT WERE NOT GIVEN A LIST, IF THERE WAS A VARIABLE 
  ##   USED IN IT'S PLACE, USE THE NAME OF THE VARIABLE
  ## OTHERWISE, LEAVE BLANK
  ## ---------------------------------------------------------------------------- ##
  ## Check if only one item was sent to (...) and it is a list.
  ## If not, convert it to list
  dots_is_a_list <- is.list(..1) && length(list(...)) == 1
  if (dots_is_a_list) {
      creds <- ..1
  } else {
    creds <- list(...)
    nms_from_vars <- as.character(substitute(as.list(...)))[-1]
    nms <- names(creds)

    ## If the names vector is null, set them to ""
    if (is.null(nms))
      nms <- rep("", length(creds))

    ## If the names of creds are blank, use the nms_from_vars UNLESS
    ##     the nms_from_vars is the same as the actual value of the cred
    ##     or the nms_from_vars contains square brackets
    ##     (in which case, leave as "")
    use_nms_from_vars <- (nms == "")  &  (nms_from_vars != as.character(creds))  &  !(grepl("\\[|\\]", nms_from_vars))

    names(creds) <- ifelse(use_nms_from_vars, nms_from_vars, nms)
  }
  ## ---------------------------------------------------------------------------- ##



  creds_as_json_object <- jsonlite::toJSON(creds)

  ## For AWS encryption, text input must be a multiple of 16 bytes
  ## Therefore, convert it to RAW, then padd it with zeros at the end

  # convert to RAW
  creds_json_as_raw <- charToRaw(creds_as_json_object)
  # calculate number of zeros needed, based on length modulo 16
  fill_zeros <- as.raw(rep(0, 16 - length(creds_json_as_raw) %% 16))
  # padd in the zeros
  creds_json_as_raw %<>% {c(., fill_zeros)}

  ## CREATE THE ENCRYPTION
  aes_encryptor <- digest::AES(key=key, mode="ECB")
  creds_encrypted <- aes_encryptor$encrypt(creds_json_as_raw)

  ## Write to disk
  writeBin(creds_encrypted, con=file_full_path)
}

#' @export
readCredentialsFromFile <- function(
  # file=".credentials.creds", key=readKeyFromFile()) {
    file_full_path = file.path(folder, file_name)
  , file_name      = getOption("rcreds.file_name", default=".credentials.creds")
  , folder         = getOption("rcreds.folder",    default="~/.rcreds/credential_files")
  , key            = readKeyFromFile()
) {
  stopifnot(requireNamespace("digest"))
  stopifnot(requireNamespace("jsonlite"))

  if ((!missing(file_name) || !missing(folder))  &&  !missing(file_full_path))
    warning("Parameters 'file_name' and 'folder' are ignored when 'file_full_path' is set explicitly")

  ## Read in the binary data
  dat <- readBin(con=file_full_path, what="raw", n=1e6)

  ## Decrypt the data to text
  aes_encryptor <- digest::AES(key=key, mode="ECB")
  creds <- aes_encryptor$decrypt(cipher=dat, raw=TRUE)

  ## &&&&&&&&&&&&  TODO &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  ## Since we had padded with zeros, remove those
  ## TODO:  only remove zeros from start of creds
        # zeros <- creds == 0
  creds <- creds[creds > 0]
  json <- rawToChar(creds[creds>0])

  ret <- jsonlite::fromJSON(json)

  return(ret)
}



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


#' @export
show_default_rcreds_file <- function() {
  file_name          = getOption("rcreds.file_name", default=".credentials.creds")
  folder             = getOption("rcreds.folder",    default="~/.rcreds/credential_files")
  file_full_path     = file.path(folder, file_name)

  return(file_full_path)
}

#' @export
show_default_rcreds_key_file <- function() {
  file_name          = getOption("rcreds.key.file_name", default=".crypt_key.rds")
  folder             = getOption("rcreds.key.folder",    default="~/.rcreds/key_files")
  file_full_path     = file.path(folder, file_name)

  return(file_full_path)
}

