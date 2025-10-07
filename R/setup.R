.onAttach <- function(libname, pkgname) {
  if (!rolog::rolog_ok())
    stop("Could not attach R package rolog.")
    
  module <- system.file(file.path("prolog", "rint.pl"), package = "intarith")
  rolog::consult(module)
}
