#' R CMD Check compalin blocker
#' 
'.' <- "dummy variable so that R CMD check will not complain"

## hmmm... this doesn't work for some reason, so using the above solution instead
# #' @importFrom utils suppressForeignCheck
# utils::suppressForeignCheck(c("."))


#' Standard onLoad function for rcreds
#' 
#' Sets several options, if they are not already set when this function funs.
#' 
#' @param libname libname and pkgname are default \code{R} \code{.onLoad()} parameters
#' @param pkgname libname and pkgname are default \code{R} \code{.onLoad()} parameters
#' 
#' @return invisible(TRUE)
#' 
#' (not exported)
.onLoad <- function(libname, pkgname) {
  ## -------------------- OPTIONS FOR PACKAGE ----------------------- ##
  ## Options to Load.  
  ##   All Values should be quoted strings
  ##   Actual string values should have quotes inside the quotes
  ##     eg  "\"an example string\"" or "'an example string'"
  ## Code adapted from the data.table package
  opts = c(  
            ## Default Location of binary credential-files
              "rcreds.file_name"      = "'.credentials.creds'"
 
            ## Default Location of Keys
            , "rcreds.key.file_name"  = "'.crypt_key.rds'"

            , "rcreds.db.file_name"   = "'.db_credentials.creds'"

            , "verbose.rcreds"        = "TRUE"
          )
  for (i in setdiff(names(opts),names(options()))) {
      eval(parse(text=paste("options(",i,"=",opts[i],")",sep="")))
  }
  ## ---------------------------------------------------------------- ##


  ## What else should happen when this package loads? 
  # "... more stuff ..."
  
  return(invisible(TRUE))
}
