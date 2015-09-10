
EigenBenchmark <- function(N, n, trim=0.1) {
    a <- getSymMatrix(N)
    traw <- replicate(n, system.time(eigen(a, symmetric = TRUE))[3])
    tmean <- mean(traw,trim=trim)
    rm(a)
    gc()
    return(tmean)
}

gpuEigenBenchmark <- function(N, n, type, trim=0.1) {
    a <- getSymGPUmatrix(N, type)
    traw <- replicate(n, system.time(eigen(a, symmetric = TRUE))[3])
    tmean <- mean(traw,trim=trim)
    rm(a)
    gc()
    return(tmean)
}

vclEigenBenchmark <- function(N, n, type, trim=0.1) {
    a <- getSymVCLmatrix(N, type)
    traw <- replicate(n, system.time(eigen(a, symmetric = TRUE))[3])
    tmean <- mean(traw,trim=trim)
    rm(a)
    gc()
    return(tmean)
}

ArmaEigenBenchmark <- function(N, n, type, trim=0.1) {
    a <- getSymMatrix(N)
    traw <- switch(type,
    							 "double" = replicate(n, system.time(arma_deigen(a))[3]),
    							 "float" = replicate(n, system.time(arma_seigen(a))[3]),
    							 stop("unrecognized type")
    							 )
    tmean <- mean(traw,trim=trim)
    rm(a)
    gc()
    return(tmean)
}

EigenEigenBenchmark <- function(N, n, type, trim=0.1) {
    a <- getSymMatrix(N)
    traw <- switch(type,
    							 "double" = replicate(n, system.time(eigen_deigen(a))[3]),
    							 "float" = replicate(n, system.time(eigen_seigen(a))[3]),
    							 stop("unrecognized type")
    							 )
    tmean <- mean(traw,trim=trim)
    rm(a)
    gc()
    return(tmean)
}

#' @export
benchmark_eigen <- function(N=3, type="double")
{
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
					 "double" = "Double Eigen Performance",
					 "float" = "Single Eigen Performance",
					 stop("Unrecongnized type")
		)
	
	for(i in 1:12){
		benchmarks[i,1] <- ArmaEigenBenchmark(ORDER[i], 3, type)
		benchmarks[i,2] <- EigenEigenBenchmark(ORDER[i], 3, type)
		benchmarks[i,3] <- gpuEigenBenchmark(ORDER[i], 3, type)
		benchmarks[i,4] <- vclEigenBenchmark(ORDER[i], 3, type)
		if(type == "double") benchmarks[i,5] <- EigenBenchmark(ORDER[i], 3)
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

