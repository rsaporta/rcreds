#' @export
hash_pw <- function(pw, algo) {
  requireNamespace("digest")

  if (!is.character(pw) || !length(pw) || !nzchar(pw))
    stop("'pw' must non-empty string")

  digest::digest(pw, algo=algo, serialize=FALSE, length=Inf, file=FALSE)
}

#' @export
hash_pw_md5 <- function(pw) {
  hash_pw(pw=pw, algo="md5")
}

#' @export
hash_pw_sha1 <- function(pw) {
  hash_pw(pw=pw, algo="sha1")
}

#' @export
hash_pw_sha512 <- function(pw) {
  hash_pw(pw=pw, algo="sha512")
}


