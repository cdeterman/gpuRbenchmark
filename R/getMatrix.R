
#' @export
getMatrix <- function(N) {
	a <- matrix(rnorm(N*N), N, N)
	invisible(gc())
	invisible(a)
}

#' @export
getGPUmatrix <- function(N, type){
	a <- gpuMatrix(rnorm(N*N), ncol=N, nrow=N, type=type)
	invisible(gc())
	invisible(a)
}

#' @export
getVCLmatrix <- function(N, type){
	a <- vclMatrix(rnorm(N*N), ncol=N, nrow=N, type=type)
	invisible(gc())
	invisible(a)
}

#' @export
getSymMatrix <- function(N) {
	a <- matrix(rnorm(N*N), N, N)
	b <- tcrossprod(a)
	invisible(gc())
	invisible(b)
}

#' @export
getSymGPUmatrix <- function(N, type){
	a <- gpuMatrix(rnorm(N*N), ncol=N, nrow=N, type=type)
	b <- tcrossprod(a)
	invisible(gc())
	invisible(b)
}

#' @export
getSymVCLmatrix <- function(N, type){
	a <- vclMatrix(rnorm(N*N), ncol=N, nrow=N, type=type)
	b <- tcrossprod(a)
	invisible(gc())
	invisible(b)
}

