.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "cheerfulgif-assets",
    system.file("www", package = pkgname)
  )
}
