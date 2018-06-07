.onLoad <- function(libname, pkgname)
{
	cat("called onLoad")
	path <- capture.output(float:::ldflags())
	Sys.setenv(PATH = paste(path, Sys.getenv("PATH"), sep = ";"))
}

.onAttach <- function(libname, pkgname)
{
	path <- capture.output(float:::ldflags())
	Sys.setenv(PATH = paste(path, Sys.getenv("PATH"), sep = ";"))
}