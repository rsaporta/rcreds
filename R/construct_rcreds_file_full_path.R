#' @export
#' @example get_rcreds_file(info="hi")
#' @example get_rcreds_file(info="hi", file_name = "world.exe")
#' @example get_rcreds_file(info="hi", file_name = ".world.exe")
construct_rcreds_file_full_path <- function(
    file_name          = getOption("rcreds.file_name", default=".credentials.creds")
  , folder             = getOption("rcreds.folder",    default="~/.rcreds/credential_files")
  , info.file_name     = ""
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
