# ========================================================== #
#    hash_pw.R                                               #
# ========================================================== #
#
#' Hashing functions
#'
#' Hashes passwords using a variety of algorithms
#'
#' Wrappers to digest package, using algorithsm made available via digest.
#' Specifically, does \emph{not} serialize the hash.
#'
#' @name hash_pw
#'
#X## -------------------------------  PARAMS  ------------------------------- ##
#' @param pw string to hash. Cannot be blank or blank-like
#'
#' @param algo algorithm to use. Passed to the digest `algo` parameter
#'             see `?digest::digest` for more.
#'
#X## ------------------------------------------------------------------------ ##
#'
#' @return
#'    The hashed string.
#' 
#' @examples
#'
#' #'  \dontrun{
#'    library(rcreds)
#' 
#'    hash_pw_md5("P4ssword!")
#'    hash_pw("P4ssword!", algo="md5")
#' 
#' }
#'
NULL


#' @rdname hash_pw
#' @importFrom magrittr %>%
#' @export
hash_pw <- function(pw, algo) {
  requireNamespace("digest")

  if (!is.character(pw) || !length(pw) || !nzchar(pw))
    stop("'pw' must non-empty string")

  digest::digest(pw, algo=algo, serialize=FALSE, length=Inf, file=FALSE)
}

#' @rdname hash_pw
#' @importFrom magrittr %>%
#' @export
hash_pw_md5 <- function(pw) {
  hash_pw(pw=pw, algo="md5")
}

#' @rdname hash_pw
#' @importFrom magrittr %>%
#' @export
hash_pw_sha1 <- function(pw) {
  hash_pw(pw=pw, algo="sha1")
}

#' @rdname hash_pw
#' @importFrom magrittr %>%
#' @export
hash_pw_sha512 <- function(pw) {
  hash_pw(pw=pw, algo="sha512")
}

