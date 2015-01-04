.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("\nThis is prideR version",packageVersion("prideR"),"\n",
          " Read '?prideR' and references therein for information\n",
          " about the package and how getting started.\n"))
}