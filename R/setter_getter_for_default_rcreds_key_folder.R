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

#' @return
#'    The currently set folder, invisibly.
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

#' @return
#'    The previously set folder, invisibly.
clear_default_rcreds_key_folder <- function(verbose=TRUE) {
  previous <- getOption("rcreds.key.folder")
  options(rcreds.key.folder = NULL)

  if (verbose) {
    previous %>% {if (is.null(.)) "not set" else sprintf("set to  '%s'", .)} %>% 
    cat("Clearing default rcreds_key_folder, which was previously", ., "\n")
  }

  return(invisible(previous))
}
