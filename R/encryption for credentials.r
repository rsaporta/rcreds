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
createKey <- function(bytes=32, depth=8, seed=NULL) {
  ## Don't set seed unless given explicitly
  if (!is.null(seed))
    set.seed(seed)

  if (!(bytes %in% c(16, 24, 32)))
    warning("bytes should generally be either 16, 24, 32")

  seq(from=0, to=2^depth-1) %>%
    sample(size=32, replace=TRUE) %>%
    as.raw()
}

#' @export
saveKey <- function(file=".crypt_key.rds", key, bytes=32, depth=8, seed=NULL) {

  if (missing(key))
    key <- createKey(bytes=bytes, depth=depth, seed=seed)
  else if (!missing(bytes) |  !missing(depth)  |  !missing(seed))
      warning("arguments 'bytes', 'depth', and 'seed' are only used when 'key' is missing. Otherwise they are ignored.")

  if (!file.exists(dirname(file)))
    dir.create(dirname(file), showWarnings=FALSE, recursive=TRUE)

  saveRDS(key, file=file)

  return(invisible(key))
}

#' @export
readKeyFromFile <- function(file=".crypt_key.rds", dont_create=FALSE) {
  if (!file.exists(file)) {
    message("Key File does not exist.  Will create one")
    saveKey(file=file)
  }

  readRDS(file)
}

#' @export
writeCredentialsToFile <- function(..., file=".credentials.creds", key=readKeyFromFile()) {
  stopifnot(requireNamespace(digest))
  stopifnot(requireNamespace(jsonlite))

  if (!file.exists(dirname(file)))
    dir.create(dirname(file), showWarnings=FALSE, recursive=TRUE)

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
  writeBin(creds_encrypted, con=file)
}

#' @export
readCredentialsFromFile <- function(file=".credentials.creds", key=readKeyFromFile()) {
  stopifnot(requireNamespace(digest))
  stopifnot(requireNamespace(jsonlite))

  dat <- readBin(con=file, what="raw", n=1e6)
  ## CREATE THE ENCRYPTION
  aes_encryptor <- digest::AES(key=key, mode="ECB")
  creds <- aes_encryptor$decrypt(cipher=dat, raw=TRUE)

  ## Since we had padded with zeros, remove those
  creds <- creds[creds > 0]
  json <- rawToChar(creds[creds>0])

  ret <- jsonlite::fromJSON(json)

  return(ret)
}

