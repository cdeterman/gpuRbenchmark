.onLoad <- function(libname, pkgname)
{
	cat("called onLoad")
	path <- "C:/Program Files/R/R-3.4.3/library/float/libs/x64"
	Sys.setenv(PATH = paste(normalizePath(path), Sys.getenv("PATH"), sep = ";"))
	# cat("\n")
	# cat(Sys.getenv("PATH"))
	# cat("\n")
}

.onAttach <- function(libname, pkgname)
{
	path <- "C:/Program Files/R/R-3.4.3/library/float/libs/x64"
	Sys.setenv(PATH = paste(normalizePath(path), Sys.getenv("PATH"), sep = ";"))
}