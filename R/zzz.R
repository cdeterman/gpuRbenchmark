.onLoad <- function(libname, pkgname)
{
	cat("called onLoad")
	path <- float:::ldflags()
	Sys.setenv(PATH = paste(path, Sys.getenv("PATH"), sep = ";"))
}

.onAttach <- function(libname, pkgname)
{
	path <- float:::ldflags()
	Sys.setenv(PATH = path, Sys.getenv("PATH"), sep = ";")
}