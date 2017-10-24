# ========================================================== #
#    setter_getter_for_default_folders.R                     #
# ========================================================== #
#

#' Setter Getter for Default rcreds Folders
#'
#' Where will your keys and credentials be saved to and read from
#'
#' Ideally, rcreds will be written to \code{~/.rcreds/}, but 
#'   the package cannot set that as a default. The user must do so.
#'
#' @name setters_getters
#'
#X## -------------------------------  PARAMS  ------------------------------- ##
#' @param DB TRUE/FALSE flag. Is the folder for the db credentials functions?
#'
#'           Defaults to: FALSE
#'
#' @param folder full path/to/folder where to store creds or key. A quoted string
#' @param parent_folder full path/to/parent_folder in which three subfolders will 
#'                      be (optionally) created to store credentials and keys
#'
#' @param check_if_exists TRUE/FALSE flag. Should we check if the folder exists
#'
#'                        Defaults to: TRUE
#'
#' @param fail_if_not_set TRUE/FALSE flag. Should an error be thrown if 
#'                         the option is not set?
#'
#'                        Defaults to: TRUE
#'
#' @param create_if_not_exist If folder does not exist, should it be created.
#'
#'                            Defaults to: FALSE
#' 
#' @param showWarnings When FALSE, warnings will be supressed.
#'
#'                     Defaults to: TRUE
#'
#' @param verbose When FALSE, output will be supressed.
#'
#'                Defaults to: TRUE
#' 
#X## ------------------------------------------------------------------------ ##
#'
#' @return
#'    The folder set in the options. 
#'
#'     for the \code{set_default_..} functions, the folder is returned invisibly.
#'
#'     for the \code{clear_default_..} functions, the previously set value, invisibly.
#'
#' @examples
#' 
#'  \dontrun{
#'    library(rcreds)
#' 
#'    set_default_rcreds_folder("~/.rcreds/credential_files")
#'    creds_folder <- get_default_rcreds_folder()
#'    creds_folder
#'
#'    set_default_rcreds_folder("~/.rcreds/db_credential_files", DB=TRUE)
#'    db_creds_folder <- get_default_rcreds_folder(DB=TRUE)
#'    db_creds_folder
#' 
#'    set_default_rcreds_key_folder("~/.rcreds/key_files")
#'    rcreds_key_folder <- get_default_rcreds_key_folder()
#'    rcreds_key_folder
#'  
#'    ## ---------------------------------------------- ##
#' 
#'    ## Alternatively, set them all in one shot
#'    set_default_rcreds_ALL(parent_folder = "~/.rcreds")
#' 
#'    ## All three values will be set
#'    get_default_rcreds_folder()
#'    get_default_rcreds_folder(DB=TRUE)
#'    get_default_rcreds_key_folder()
#'  }
#'
NULL



#' @rdname setters_getters
#' @importFrom magrittr %>%
#' @export
get_default_rcreds_folder <- function(DB=FALSE, check_if_exists=TRUE, fail_if_not_set=TRUE, showWarnings=TRUE) {

  ## VALIDATE INPUTS
  ## ------------------------------------------------------------------- ##
  .confirm_is_logical_of_length1(DB,              NAs_ok=FALSE)
  .confirm_is_logical_of_length1(check_if_exists, NAs_ok=FALSE)
  .confirm_is_logical_of_length1(fail_if_not_set, NAs_ok=FALSE)
  .confirm_is_logical_of_length1(showWarnings,    NAs_ok=FALSE)
  ## ------------------------------------------------------------------- ##

  suggested <- "~/.rcreds/%scredential_files" %>% sprintf(ifelse(DB, "db_", ""))

  if (DB) 
    ret <- getOption("rcreds.db_folder")
  else
    ret <- getOption("rcreds.folder")

  if (is.null(ret)) {
    msg <- paste(sep="\n"
      , "The default rcreds %sfolder is NOT set."
      , "Please run set_default_rcreds_folder(folder=\"path/to/folder\", DB = %s)"
      , "The suggested location is %s, ie:"
      , "     set_default_rcreds_folder(folder=\"%s\", DB = %s)"
      ) %>% 
    sprintf(ifelse(DB, "db ", ""), DB, suggested, suggested, DB)
    if (fail_if_not_set)
      stop(msg)
    if (showWarnings)
      warning(msg)
  } else {

    ## CHECK THAT EXISTS
    if (check_if_exists && !file.exists(ret)) {
      if (showWarnings)
        warning("the default rcreds ", ifelse(DB, "db ", ""), "folder is '", ret, "' but it does not exist.\n  HINT: use dir.create()")
    }

    ## CHECK THAT IT IS IN FACT A FOLDER
    if (file.exists(ret) && !file.info(ret)$isdir) {
      if (showWarnings)
        warning("the default rcreds ", ifelse(DB, "db ", ""), "folder is '", ret, "', exists BUT it is *NOT* a directory.\n You may not be able to use it as intended.")
    }
  }

  return(ret)
}

#' @rdname setters_getters
#' @importFrom magrittr %>%
#' @export
set_default_rcreds_folder <- function(folder, DB=FALSE, create_if_not_exist=FALSE, showWarnings=TRUE, verbose=TRUE) {

  ## VALIDATE INPUTS
  ## ------------------------------------------------------------------- ##
  .confirm_is_string_of_length1(folder, empty_string_ok=FALSE)

  .confirm_is_logical_of_length1(DB,                  NAs_ok=FALSE)
  .confirm_is_logical_of_length1(create_if_not_exist, NAs_ok=FALSE)
  .confirm_is_logical_of_length1(showWarnings,        NAs_ok=FALSE)
  ## ------------------------------------------------------------------- ##

  if (!file.exists(folder)) {
    if (create_if_not_exist) {
      if (verbose)
        cat("Folder does not exist. Creating it.", "\n")
      dir.create(folder, recursive=TRUE, mode="0775")
    } else if (showWarnings) {
      warning("Folder '", folder, "' does not exist")
    }
  }

  if (verbose) {
    cat(sprintf("Setting the default rcreds %sfolder to '%s'\n", ifelse(DB, "db ", ""), folder))
  }

  if (DB)
    options(rcreds.db_folder = folder)
  else
    options(rcreds.folder = folder)

  return(invisible(folder))
}

#' @rdname setters_getters
#' @importFrom magrittr %>%
#' @export
clear_default_rcreds_folder <- function(DB=FALSE, verbose=TRUE) {
  ## VALIDATE INPUTS
  ## ------------------------------------------------------------------- ##
  .confirm_is_logical_of_length1(DB,                  NAs_ok=FALSE)
  .confirm_is_logical_of_length1(verbose,             NAs_ok=FALSE)
  ## ------------------------------------------------------------------- ##

  if (DB) {
    previous <- getOption("rcreds.db_folder")
    options(rcreds.db_folder = NULL)
  } else {
    previous <- getOption("rcreds.folder")
    options(rcreds.folder = NULL)
  }

  if (verbose) {
    previous %>% {if (is.null(.)) "not set" else sprintf("set to  '%s'", .)} %>% 
    cat("Clearing default rcreds", ifelse(DB, "db", ""), "folder, which was previously", ., "\n")
  }

  return(invisible(previous))
}

#' @rdname setters_getters
#' @importFrom magrittr %>%
#' @export
get_default_rcreds_key_folder <- function(check_if_exists=TRUE, fail_if_not_set=TRUE, showWarnings=TRUE) {
  ## VALIDATE INPUTS
  ## ------------------------------------------------------------------- ##
  .confirm_is_logical_of_length1(check_if_exists, NAs_ok=FALSE)
  .confirm_is_logical_of_length1(fail_if_not_set, NAs_ok=FALSE)
  .confirm_is_logical_of_length1(showWarnings,    NAs_ok=FALSE)
  ## ------------------------------------------------------------------- ##

  suggested <- "~/.rcreds/key_files"

  ret <- getOption("rcreds.key.folder")

  if (is.null(ret)) {
    msg <- paste(sep="\n"
      , "The default rcreds key folder is NOT set."
      , "Please run set_default_rcreds_key_folder(folder=\"path/to/folder\")"
      , "The suggested location is %s, ie:"
      , "     set_default_rcreds_key_folder(folder=\"%s\")"
      ) %>% 
    sprintf(suggested, suggested)
    if (fail_if_not_set)
      stop(msg)
    if (showWarnings)
      warning(msg)
  } else {

    ## CHECK THAT EXISTS
    if (check_if_exists && !file.exists(ret)) {
      if (showWarnings)
        warning("the default rcreds key folder is '", ret, "' but it does not exist.\n  HINT: use dir.create()")
    }

    ## CHECK THAT IT IS IN FACT A FOLDER
    if (file.exists(ret) && !file.info(ret)$isdir) {
      if (showWarnings)
        warning("the default rcreds key folder, '", ret, "', exists BUT it is *NOT* a directory.\n You may not be able to use it as intended.")
    }
  }

  return(ret)
}

#' @rdname setters_getters
#' @importFrom magrittr %>%
#' @export
set_default_rcreds_key_folder <- function(folder, create_if_not_exist=FALSE, showWarnings=TRUE, verbose=TRUE) {

  ## VALIDATE INPUTS
  ## ------------------------------------------------------------------- ##
  .confirm_is_string_of_length1(folder, empty_string_ok=FALSE)
  .confirm_is_logical_of_length1(create_if_not_exist, NAs_ok=FALSE)
  .confirm_is_logical_of_length1(showWarnings,        NAs_ok=FALSE)
  .confirm_is_logical_of_length1(verbose,             NAs_ok=FALSE)
  ## ------------------------------------------------------------------- ##

  if (!file.exists(folder)) {
    if (create_if_not_exist) {
      if (verbose)
        cat("Folder does not exist. Creating it.", "\n")
      dir.create(folder, recursive=TRUE, mode="0775")
    } else if (showWarnings) {
      warning("Folder '", folder, "' does not exist")
    }
  }

  if (verbose) {
    cat(sprintf("Setting the default rcreds key folder to '%s'\n", folder))
  }

  options(rcreds.key.folder = folder)

  return(invisible(folder))
}

#' @rdname setters_getters
#' @importFrom magrittr %>%
#' @export
clear_default_rcreds_key_folder <- function(verbose=TRUE) {
  previous <- getOption("rcreds.key.folder")
  options(rcreds.key.folder = NULL)

  if (verbose) {
    previous %>% {if (is.null(.)) "not set" else sprintf("set to  '%s'", .)} %>% 
    cat("Clearing default rcreds_key_folder, which was previously", ., "\n")
  }

  return(invisible(previous))
}


#' @rdname setters_getters
#' @importFrom magrittr %>%
#' @export
set_default_rcreds_ALL <- function(parent_folder, create_if_not_exist=FALSE, showWarnings=TRUE, verbose=TRUE) {

  if (missing(parent_folder)) {
    stop(
      "parent_folder must be given explicitly.\n",
      "  HINT: recommended location is '~/.rcreds/'\n",
      "  \n",
      "  set_default_rcreds_ALL(parent_folder=\"~/.rcreds/\")         OR \n",
      "     \n",
      "  set_default_rcreds_ALL(parent_folder=\"~/.rcreds/\", create_if_not_exist=TRUE)"
    )
  }

  FOLDER_DB    <- file.path(parent_folder, "db_credential_files")
  FOLDER_CREDS <- file.path(parent_folder, "credential_files")
  FOLDER_KEY   <- file.path(parent_folder, "key_files")

  if (!file.exists(parent_folder)) {
    if (create_if_not_exist)
      dir.create(parent_folder, mode="0775")
  } else if (!file.info(parent_folder)$isdir) {
    stop("parent_folder '", parent_folder, "' exists but it is NOT a folder.\nWill not be able to create subfolders at that location.")
  }

  if (verbose) {
    whitespace <- rep(" ", nchar(parent_folder)-1) %>% paste0(collapse="")
    cat(
      "Will set", " (and possible create)", " three subfolders in the parent_folder", "\n"
    , "     ", parent_folder, "\n"
    , "     ", whitespace, "|-  /db_credential_files/", "\n"
    , "     ", whitespace, "|-  /credential_files/", "\n"
    , "     ", whitespace, "|-  /key_files/", "\n"
    , "", "\n"
    , "", sep="")
  }

  set_default_rcreds_folder(folder=FOLDER_DB,      DB = TRUE,  create_if_not_exist=create_if_not_exist, showWarnings=showWarnings, verbose=FALSE)
  set_default_rcreds_folder(folder=FOLDER_CREDS,   DB = FALSE, create_if_not_exist=create_if_not_exist, showWarnings=showWarnings, verbose=FALSE)
  set_default_rcreds_key_folder(folder=FOLDER_KEY,             create_if_not_exist=create_if_not_exist, showWarnings=showWarnings, verbose=FALSE)

  return(invisible(parent_folder))
}
