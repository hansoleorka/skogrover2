#' Source an R script from GitHub over HTTPS
#'
#' This function downloads and evaluates one or more R scripts hosted on GitHub (or any HTTPS-accessible URL).
#' It uses the RCurl package to fetch the script content and evaluates it in the global environment.
#'
#' @param url A character string specifying the URL of the R script to source.
#' @param ... Additional parameters.
#'
#' @return The result of evaluating each script, invisibly.
#'
#' @details This function is useful for dynamically loading R code from remote repositories.
#' It requires the \code{RCurl} package and uses the system's SSL certificate for secure connections.
#'
#' @examples
#' \dontrun{
#' source_github("https://raw.githubusercontent.com/user/repo/branch/script.R")
#' }
#'
#' @references \url{http://www.r-bloggers.com/source_https-sourcing-an-r-script-from-github-over-https/}
#'
#' @export
source_github <- function(url, ...) {
  # load package
  require(RCurl)
  
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}



