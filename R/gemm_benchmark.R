#' @useDynLib gpuRbenchmark
#' @importFrom Rcpp evalCpp
#' @import gpuR
#' @import reshape2
#' @import ggplot2
NULL

MatMultBenchmark <- function(N, n, trim=0.1) {
    a <- getMatrix(N)
    traw <- replicate(n, system.time(a%*%a)[3])
    tmean <- mean(traw,trim=trim)
    rm(a)
    gc()
    return(tmean)
}

gpuMatMultBenchmark <- function(N, n, type, trim=0.1) {
    a <- getGPUmatrix(N, type)
    traw <- replicate(n, system.time(a%*%a)[3])
    tmean <- mean(traw,trim=trim)
    rm(a)
    gc()
    return(tmean)
}

vclMatMultBenchmark <- function(N, n, type, trim=0.1) {
    a <- getVCLmatrix(N, type)
    traw <- replicate(n, system.time(a%*%a)[3])
    tmean <- mean(traw,trim=trim)
    rm(a)
    gc()
    return(tmean)
}

ArmaMatMultBenchmark <- function(N, n, type, trim=0.1) {
    a <- getMatrix(N)
    traw <- switch(type,
    							 "double" = replicate(n, system.time(gpuRbenchmark:::arma_dgemm(a,a))[3]),
    							 "float" = replicate(n, system.time(arma_sgemm(a,a))[3])
    							 )
    tmean <- mean(traw,trim=trim)
    rm(a)
    gc()
    return(tmean)
}

EigenMatMultBenchmark <- function(N, n, type, trim=0.1) {
    a <- getMatrix(N)
    traw <- 
    	switch(type,
    				 "double" = replicate(n, system.time(eigen_dgemm(a,a))[3]),
    				 "float" = replicate(n, system.time(eigen_sgemm(a,a))[3]),
    				 stop("unrecognized type")
    	)
    tmean <- mean(traw,trim=trim)
    rm(a)
    gc()
    return(tmean)
}

#' @export
benchmark_gemm <- function(N = 3, type = "double"){
	if(type == "double"){
		benchmarks = matrix(0, nrow=12, ncol=5)
		colnames(benchmarks) <- c("armadillo", "eigen", "gpuMatrix", "vclMatrix", "base")
	}else{
		benchmarks = matrix(0, nrow=12, ncol=4)
		colnames(benchmarks) <- c("armadillo", "eigen", "gpuMatrix", "vclMatrix")
	}
	
	ORDER <- c(10, 100, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000)
	
	title <- 
		switch(type,
					 "double" = "DGEMM Performance",
					 "float" = "SGEMM Performance",
					 stop("Unrecongnized type")
		)
	
	for(i in 1:12){
		benchmarks[i,1] <- ArmaMatMultBenchmark(ORDER[i], N, type)
		benchmarks[i,2] <- EigenMatMultBenchmark(ORDER[i], N, type)
		benchmarks[i,3] <- gpuMatMultBenchmark(ORDER[i], N, type)
		benchmarks[i,4] <- vclMatMultBenchmark(ORDER[i], N, type)
		if(type == "double") benchmarks[i,5] <- MatMultBenchmark(ORDER[i], N)
	}
	
	# add matrix order
	benchmarks <- as.data.frame(cbind(benchmarks, ORDER))
	
	# reshape to long format
	df <- melt(as.data.frame(benchmarks), id = c("ORDER"))
	
	# plot
	p <- ggplot(df, aes(y = value, x = ORDER, group = variable, colour=variable)) 
	p <- p + geom_line() 
	p <- p + ggtitle(title)
	p <- p + scale_y_continuous(name = "time (sec)")
	p
}
