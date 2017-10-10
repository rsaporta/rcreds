.onLoad <- function(libname, pkgname, verbose=getOption("verbose.onLoad", default=TRUE)) {
  if (verbose)
    cat("Running .onLoad() from ", pkgname, "  ||   [turn off messaging with options(verbose.onLoad = FALSE) ]\n")

  ## -------------------- OPTIONS FOR PACKAGE ----------------------- ##
  ## Options to Load.  
  ##   All Values should be quoted strings
  ##   Actual string values should have quotes inside the quotes
  ##     eg  "\"an example string\"" or "'an example string'"
  ## Code adapted from the data.table package
  opts = c(  "rcreds.folder"       = "'~/.ssh/rcreds/'"
           , "rcreds.file_name"    = "'.credentials.creds'"
          )
  for (i in setdiff(names(opts),names(options()))) {
      eval(parse(text=paste("options(",i,"=",opts[i],")",sep="")))
  }
  ## ---------------------------------------------------------------- ##


  ## What else should happen when this package loads? 
  # "... more stuff ..."
  
  return(invisible(TRUE))
}


