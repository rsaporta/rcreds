get_default_rcreds_folder <- function(DB=FALSE, fail_if_not_set=TRUE, showWarnings=TRUE) {
  .confirm_is_logical_of_length1(DB, NAs_ok=FALSE)
  .confirm_is_logical_of_length1(showWarnings, NAs_ok=FALSE)

  suggested <- "~/.rcreds/%scredential_files" %>% sprintf(ifelse(db, "db_", ""))

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
  }

  return(ret)
}

set_default_rcreds_folder <- function(folder, DB=FALSE, create_if_not_exist=FALSE, showWarnings=TRUE, verbose=TRUE) {
  .confirm_is_string_of_length1(folder, empty_string_ok=FALSE)
  .confirm_is_logical_of_length1(DB, empty_string_ok=FALSE)
  .confirm_is_logical_of_length1(create_if_not_exist, empty_string_ok=FALSE)
  .confirm_is_logical_of_length1(showWarnings, NAs_ok=FALSE)

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
    cat(sprintf("Setting the %sfolder to '%s'\n", ifelse(DB, "db ", ""), folder))
  }

  if (DB)
    options(rcreds.db_folder = folder)
  else
    options(rcreds.folder = folder)

  return(invisible(folder))
}



### WORK IN PROGRESS
stop("you need to add setters / getters to vignette")

also -- check what you do in onLoad