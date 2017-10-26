# ========================================================== #
#    credentials_functions.r                                 #
# ========================================================== #
#
#' Credentials Functions
#'
#' Securely Write/Read Sensitive Parameters to/from Disk
#'
#' #' There are two sets of pairs of functions
#' use \code{write_credentials_to_file()} to output to disk
#' use \code{read_credentials_from_file()} to read in the credentials back to R
#' 
#' Similarly, there are a pair of functions with the 5 comonly-used parameters for database connections
#' use \code{write_db_credentials_to_file()} and \code{read_db_credentials_from_file()}
#' 
#'
#'
#' @name credentials_functions
#'
#X## -------------------------------  PARAMS  ------------------------------- ##
#' @param file_full_path The full path to the creds (or key) file, where it should be read from or written to. 
#'     if \code{"..auto.."} then will be constructed from \code{folder}, \code{file_name}, and \code{info.file_name}
#'     
#'     NOTE: When \code{file_full_path} is set explicitly, then \code{folder}, \code{file_name}, and \code{info.file_name} are ignored.
#'
#'                       Defaults to: "..auto.."
#'
#' @param info.file_name Will be added as a prefix to the filename. 
#' 
#' Useful when using multiple files in a given folder. 
#'
#'                       Defaults to: "\\"\\""
#'
#' @param file_name name of the file where the credentials will be written to or read from.  Should be a string of length 1
#'
#'                  Defaults to: getOption(\\"rcreds.file_name\\", default = \\".credentials.creds\\")
#'
#' @param folder folder where the credentials will be written to or read from.
#'
#'               Defaults to: get_default_rcreds_folder(DB=FALSE)
#'
#' @param key A key object of class \code{"key_rcreds"} to be used for encrypting / decrypting. Passed to \code{digest::AES}.  
#' 
#'                 Alternatively, a full file path to a key stored on disk can be given which will be read to disk.
#' 
#'            Defaults to: read_key_from_file()
#'
#' @param ... values to be encrypted and written to the credentials file. 
#'            if named parameters, the list which is outputed by \code{read_credentials_from_file} will use those same names.
#' 
#'            if empty in \code{write_credentials_to_file} then nothing will be written to disk in the credentials file
#' 
#' @param dbname parameter for database connections. Will be encrypted and written to database
#'
#'               Defaults to: "dev"
#'
#' @param host parameter for database connections. Will be encrypted and written to database
#'
#'             Defaults to: "localhost"
#'
#' @param port parameter for database connections. Will be encrypted and written to database
#'
#'             Defaults to: 5432
#'
#' @param username parameter for database connections. Will be encrypted and written to database
#'
#'                 Defaults to: "you_forgot_to_specify_username"
#'
#' @param password parameter for database connections. Will be encrypted and written to database
#'
#'                 Defaults to: "too_many_secrets"
#'
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
#' @param fail_if_cant_decrypt A TRUE/FALSE flag.  If set to TRUE, the reading functions will fail on error. If set to FALSE, NULL will be returned and a graceful exit will happen (with a possible warning if \code{showWarnings} is TRUE.
#'
#'                           Defaults to: TRUE
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
#' Details
#' 
#' The \code{write_..} functions take a list of parameters along with a key object, encrypt the parameters and write them to a file on disk.
#' 
#' The \code{read_..} functions read said file, and given (the same) key object, decrypt the parameters and return a named list. 
#' 
#' The corresponding \code{.._db_..} files are wrappers that explicitly list the main five parameters used for database connections, with comonly used defaults. Namely, \code{host}, \code{username}, \code{password}, \code{port}, and \code{database}.
#' 
#' 
#' @return
#' for \code{write_credentials_to_file} and \code{write_db_credentials_to_file} The file path where the encrypted values have been stored, reutrned invisibly. ie the value of \code{file_full_path}
#' 
#' for \code{read_credentials_from_file} and \code{read_db_credentials_from_file} a named list of the values stored in the credentials file. The names of the list correspond to the names of the argument passed to the corresponding write functions
#'
#' @examples
#' 
#'  \dontrun{
#'    library(rcreds)
#'  
#'    some_login_function <- function(username, password) {
#'      ## does something with username/password
#'      ## ... 
#'    }
#'  
#'    ### ---------------------------------------------- ###
#'    ## Default Folders need to be set. This shold be in an .Rprofile file
#'    ### ---------------------------------------------- ###
#'    ## generally use:  set_default_rcreds_ALL(parent_folder = "~/.rcreds/")
#'    set_default_rcreds_ALL(parent_folder = file.path(tempdir(), ".rcreds/"), 
#'                           create_if_not_exist = TRUE)
#'    ### ---------------------------------------------- ###
#'  
#'    ## ONE TIME, DO NOT SAVE THIS 
#'    write_db_credentials_to_file(username="cosmo", password="still too many secrets"
#'                               , port=1234, host="ec2-1234-567-89.us-west.compute.amazonaws.com")
#'  
#'  
#'    ## SEPARATELY, in a new file:
#'    credentials_list <- read_db_credentials_from_file(fail_if_cant_decrypt=FALSE, showWarnings=FALSE)
#'    ## normally, leave the above flags as their default TRUE. Using FALSE for this example only.
#'  
#'    some_login_function(username = credentials_list$user_name
#'                      , password = credentials_list$password
#'                       )
#'  }
#' 
#'
NULL


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

#' @rdname credentials_functions
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @export
write_credentials_to_file <- function(
    ...
  , file_full_path     = "..auto.."
  , info.file_name     = ""
  , file_name          = getOption("rcreds.file_name", default=".credentials.creds")
  , folder             = get_default_rcreds_folder(DB=FALSE)
  , allow_root_user    = FALSE
  , zArchive_existing  = TRUE
  , overwrite_existing = FALSE
  , key                = read_key_from_file()
  , showWarnings       = TRUE
  , verbose            = getOption("verbose.rcreds", default=TRUE)
) {

  .stop_if_root(allow_root_user=allow_root_user)

  stopifnot(requireNamespace("digest"))
  stopifnot(requireNamespace("jsonlite"))

  ## if key is a file, this will read it to disk. If it is an object, this will validate it.
  key %<>% use_key()

  ## Validate input
  .confirm_is_string_of_length1(file_full_path, "file_full_path", empty_string_ok=FALSE, NAs_ok=FALSE)

  ## Create 'file_full_path' if auto.  Otherwise, warn when any pieces were given explicitly.
  ## --------------------------------------------------------------------------------- ##
  if (isTRUE(file_full_path == "..auto..")) {
    file_full_path <- construct_rcreds_file_full_path(file_name=file_name, folder=folder, info.file_name=info.file_name)
  } else if (!missing(file_full_path)) {
      if ((!missing(file_name) || !missing(folder)) || !missing(info.file_name))
        warning("Parameters 'file_name', 'folder', and 'info.file_name' are ignored when 'file_full_path' is set explicitly")
  }
  ## --------------------------------------------------------------------------------- ##


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
  ## THOSE THAT WERE NOT GIVEN A NAME, IF THERE WAS A VARIABLE 
  ##   USED IN IT'S PLACE, USE THE NAME OF THE VARIABLE
  ## ^^^^^  TODO -- warn on this ^^^^^^^^^^^
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


  if(.is_any_element_NULL(creds) && showWarnings)
    warning("At least one element being written is NULL. Upon reading the file back into R, the NULLs will appear as `named list()`")

  creds_as_json_object <- jsonlite::toJSON(creds)

  ## For AWS encryption, text input must be a multiple of 16 bytes
  ## Therefore, convert it to RAW, then padd it with zeros at the end

  # convert to RAW
  creds_json_as_raw <- charToRaw(creds_as_json_object)
  # calculate number of zeros needed, based on length modulo 16
  fill_zeros <- as.raw(rep(0, 16 - length(creds_json_as_raw) %% 16))
  # padd in the zeros
  creds_json_as_raw %<>% c(fill_zeros)

  ## CREATE THE ENCRYPTION
  aes_encryptor <- digest::AES(key=key, mode="ECB")
  creds_encrypted <- aes_encryptor$encrypt(creds_json_as_raw)

  ## Write to disk
  writeBin(creds_encrypted, con=file_full_path)

  if (verbose)
    message("credentials written to file  \"", file_full_path, "\"")

  return(invisible(file_full_path))
}

#' @rdname credentials_functions
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
read_credentials_from_file <- function(
    file_full_path     = "..auto.."
  , info.file_name     = ""
  , file_name          = getOption("rcreds.file_name", default=".credentials.creds")
  , folder             = get_default_rcreds_folder(DB=FALSE)
  , key                = read_key_from_file()
  , fail_if_cant_decrypt = TRUE
  , showWarnings       = TRUE
  , verbose            = getOption("verbose.rcreds", default=TRUE) ## not yet used as of 2017-10-24
) {
  stopifnot(requireNamespace("digest"))
  stopifnot(requireNamespace("jsonlite"))

  ## if key is a file, this will read it to disk. If it is an object, this will validate it.
  key %<>% use_key()

  ## Validate input
  .confirm_is_string_of_length1(file_full_path, "file_full_path", empty_string_ok=FALSE, NAs_ok=FALSE)

  ## Create 'file_full_path' if auto.  Otherwise, warn when any pieces were given explicitly.
  ## --------------------------------------------------------------------------------- ##
  if (file_full_path == "..auto..") {
    file_full_path <- construct_rcreds_file_full_path(file_name=file_name, folder=folder, info.file_name=info.file_name)
  } else if (!missing(file_full_path)) {
      if ((!missing(file_name) || !missing(folder)) || !missing(info.file_name))
        warning("Parameters 'file_name', 'folder', and 'info.file_name' are ignored when 'file_full_path' is set explicitly")
  }
  ## --------------------------------------------------------------------------------- ##

  ## Read in the binary data
  dat <- readBin(con=file_full_path, what="raw", n=1e6)

  ## Decrypt the data to text
  aes_encryptor <- digest::AES(key=key, mode="ECB")
  creds <- aes_encryptor$decrypt(cipher=dat, raw=TRUE)

  ## Since we had padded with zeros, remove those
  ## only remove zeros from start of creds
  creds <- creds[cumsum(creds) > 0]
  json <- try(rawToChar(creds), silent=TRUE)

  ## If a bad key, the parsing might fail on rawToChar, specifically with errors like "embeded nul in string"
  ## Capture the error and assign to ret, which will be handled by the user-defined error handling
  if (inherits(json, "try-error")) {
    ret <- json
  } else {
    ret <- try(jsonlite::fromJSON(json), silent=TRUE)
  }


  if (inherits(ret, "try-error")) {
    err_msg <- as.character(ret)
    if (grepl("(invalid char in json text|embedded nul in string: )", err_msg))
      msg <- "Could not decrypt the file. This is probably due to the wrong key being used."
    else
      msg <- paste0("Parsing the text failed with the following error:\n\n", err_msg)

    if (fail_if_cant_decrypt)
      stop(msg)
    else if (showWarnings)
      warning(msg)

    return(NULL)
  }

  return(ret)
}

