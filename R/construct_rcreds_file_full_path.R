#' Construct Rcreds File Full Path
#'
#' Takes parts of a file name, info, and folder and creates a full path
#'
#' @param file_name       string, cannot be empty  Filename with extension. Default is '.credentials.creds'
#' @param folder          string, cannot be empty  Where file will be stored
#' @param info.file_name  string, can    be empty. Prepended to file_name.            
#' @param DB TRUE/FALSE flag. Is the folder for the db credentials functions?
#'
#'           Defaults to: FALSE
#'
#' @return 
#'     The full file path
#' 
construct_rcreds_file_full_path <- function(
    file_name          = getOption("rcreds.file_name", default=".credentials.creds")
  , folder             = get_default_rcreds_folder(DB=DB)
  , info.file_name     = ""
  , DB                 = FALSE
) {

  ## validate input
  .confirm_is_string_of_length1(file_name)
  .confirm_is_string_of_length1(folder)
  .confirm_is_string_of_length1(info.file_name, empty_string_ok=TRUE)

  ## identify sep to use between info.file_name and file_name
  ## ie,  file_name generally starts with a dot, in which case, no sep is needed.
  use_info_sep <- nzchar(info.file_name) & !grepl(pattern="^\\.", x=file_name)
  sep <- ifelse(use_info_sep, ".", "")

  ## construct full path to file
  file_full_path <- paste(info.file_name, file_name, sep=sep) %>% 
                    file.path(folder, .)

  return(file_full_path)
}










