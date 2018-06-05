#' @useDynLib gpuRbenchmark
#' @importFrom Rcpp evalCpp
#' @import gpuR
#' @import microbenchmark
#' @import ggplot2
NULL



# MatMultBenchmark <- function(N, n, trim=0.1) {
#     a <- getMatrix(N)
#     traw <- replicate(n, system.time(a%*%a)[3])
#     tmean <- mean(traw,trim=trim)
#     rm(a)
#     gc()
#     return(tmean)
# }
# 
# gpuMatMultBenchmark <- function(N, n, type, trim=0.1) {
#     a <- getGPUmatrix(N, type)
#     traw <- replicate(n, system.time(a%*%a)[3])
#     tmean <- mean(traw,trim=trim)
#     rm(a)
#     gc()
#     return(tmean)
# }
# 
# vclMatMultBenchmark <- function(N, n, type, trim=0.1) {
#     a <- getVCLmatrix(N, type)
#     traw <- replicate(n, system.time(a%*%a)[3])
#     tmean <- mean(traw,trim=trim)
#     rm(a)
#     gc()
#     return(tmean)
# }
# 
# ArmaMatMultBenchmark <- function(N, n, type, trim=0.1) {
#     a <- getMatrix(N)
#     traw <- switch(type,
#     							 "double" = replicate(n, system.time(gpuRbenchmark:::arma_dgemm(a,a))[3]),
#     							 "float" = replicate(n, system.time(arma_sgemm(a,a))[3])
#     							 )
#     tmean <- mean(traw,trim=trim)
#     rm(a)
#     gc()
#     return(tmean)
# }
# 
# EigenMatMultBenchmark <- function(N, n, type, trim=0.1) {
#     a <- getMatrix(N)
#     traw <-
#     	switch(type,
#     				 "double" = replicate(n, system.time(eigen_dgemm(a,a))[3]),
#     				 "float" = replicate(n, system.time(eigen_sgemm(a,a))[3]),
#     				 stop("unrecognized type")
#     	)
#     tmean <- mean(traw,trim=trim)
#     rm(a)
#     gc()
#     return(tmean)
# }

# @export
# benchmark_gemm <- function(N = 3, type = "double"){
# 	if(type == "double"){
# 		benchmarks = matrix(0, nrow=12, ncol=5)
# 		colnames(benchmarks) <- c("armadillo", "eigen", "gpuMatrix", "vclMatrix", "base")
# 	}else{
# 		benchmarks = matrix(0, nrow=12, ncol=4)
# 		colnames(benchmarks) <- c("armadillo", "eigen", "gpuMatrix", "vclMatrix")
# 	}
# 
# 	ORDER <- c(10, 100, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000)
# 
# 	title <-
# 		switch(type,
# 					 "double" = "DGEMM Performance",
# 					 "float" = "SGEMM Performance",
# 					 stop("Unrecongnized type")
# 		)
# 
# 	for(i in 1:12){
# 		benchmarks[i,1] <- ArmaMatMultBenchmark(ORDER[i], N, type)
# 		benchmarks[i,2] <- EigenMatMultBenchmark(ORDER[i], N, type)
# 		benchmarks[i,3] <- gpuMatMultBenchmark(ORDER[i], N, type)
# 		benchmarks[i,4] <- vclMatMultBenchmark(ORDER[i], N, type)
# 		if(type == "double") benchmarks[i,5] <- MatMultBenchmark(ORDER[i], N)
# 	}
# 
# 	# add matrix order
# 	benchmarks <- as.data.frame(cbind(benchmarks, ORDER))
# 
# 	# reshape to long format
# 	df <- melt(as.data.frame(benchmarks), id = c("ORDER"))
# 
# 	# plot
# 	p <- ggplot(df, aes(y = value, x = ORDER, group = variable, colour=variable))
# 	p <- p + geom_line()
# 	p <- p + ggtitle(title)
# 	p <- p + scale_y_continuous(name = "time (sec)")
# 	p
# }


vcl_gemm <- function(A){
	ret <- A %*% A
	synchronize()
	invisible(ret)
}

vcl_crossprod <- function(A){
	ret <- crossprod(A)
	synchronize()
	invisible(ret)
}

vcl_tcrossprod <- function(A){
	ret <- tcrossprod(A)
	synchronize()
	invisible(ret)
}


#' @importFrom dplyr bind_rows
#' @export
benchmark_gemm <- function(ORDER = NULL, N = 3, type = "double"){
	
	# ORDER <- c(10, 100, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000)
	if(is.null(ORDER)){
		ORDER <- c(10, 100, 500, 1000)	
	}
	
	benchmarks <- vector("list", length = length(ORDER))
	names(benchmarks) <- ORDER
	# colnames(benchmarks) <- c("expr","min", "lq", "mean", "median", "uq", "max")
	
	
	
	title <- 
		switch(type,
					 "double" = "DGEMM Performance",
					 "float" = "SGEMM Performance",
					 stop("Unrecongnized type")
		)
	
	for(i in 1:length(ORDER)){
		
		A <- getMatrix(ORDER[i])
		gpuA <- gpuMatrix(A, type = type)
		vclA <- vclMatrix(A, type = type)
		
		mbm <- microbenchmark(base = A %*% A,
													gpu = gpuA %*% gpuA,
													vcl = vcl_gemm(vclA),
													times = N, unit = "ms")
		
		benchmarks[[i]] <- mbm
	}
	
	df <- bind_rows(lapply(seq_along(benchmarks), function(i) { 
		benches <- summary(benchmarks[[i]])[,1:8]
		benches$expr <- as.character(benches$expr)
		cbind.data.frame(ORDER = as.character(rep(names(benchmarks)[i], 3)), benches, stringsAsFactors = FALSE) 
	}))
	
	df$ORDER <- as.numeric(df$ORDER)
	
	# plot
	p <- ggplot(df, aes(y = mean, x = ORDER, group = expr, colour=expr)) + 
		geom_line() + 
		geom_errorbar(aes(ymin = lq, ymax = uq)) +
		ggtitle(title) + 
		scale_y_continuous(name = "time (sec)")
	  # scale_y_log10(name = "time (sec)")
	
	plot(p)
	return(df)
}

#' @export
benchmark_crossprod <- function(ORDER = NULL, N = 3, type = "double"){
	
	# ORDER <- c(10, 100, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000)
	if(is.null(ORDER)){
		ORDER <- c(10, 100, 500, 1000)	
	}
	
	benchmarks <- vector("list", length = length(ORDER))
	names(benchmarks) <- ORDER
	# colnames(benchmarks) <- c("expr","min", "lq", "mean", "median", "uq", "max")
	
	
	
	title <- 
		switch(type,
					 "double" = "crossprod Double Performance",
					 "float" = "crossprod Single Performance",
					 stop("Unrecongnized type")
		)
	
	for(i in 1:length(ORDER)){
		
		A <- getMatrix(ORDER[i])
		gpuA <- gpuMatrix(A, type = type)
		vclA <- vclMatrix(A, type = type)
		
		mbm <- microbenchmark(base = crossprod(A),
													gpu = crossprod(gpuA),
													vcl = vcl_crossprod(vclA),
													times = N, unit = "ms")
		
		benchmarks[[i]] <- mbm
	}
	
	df <- bind_rows(lapply(seq_along(benchmarks), function(i) { 
		benches <- summary(benchmarks[[i]])[,1:8]
		benches$expr <- as.character(benches$expr)
		cbind.data.frame(ORDER = as.character(rep(names(benchmarks)[i], 3)), benches, stringsAsFactors = FALSE) 
	}))
	
	df$ORDER <- as.numeric(df$ORDER)
	
	# plot
	p <- ggplot(df, aes(y = mean, x = ORDER, group = expr, colour=expr)) + 
		geom_line() + 
		geom_errorbar(aes(ymin = lq, ymax = uq)) +
		ggtitle(title) + 
		scale_y_continuous(name = "time (sec)")
	# scale_y_log10(name = "time (sec)")
	
	plot(p)
	return(df)
}


#' @export
benchmark_tcrossprod <- function(ORDER = NULL, N = 3, type = "double"){
	
	# ORDER <- c(10, 100, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000)
	if(is.null(ORDER)){
		ORDER <- c(10, 100, 500, 1000)	
	}
	
	benchmarks <- vector("list", length = length(ORDER))
	names(benchmarks) <- ORDER
	# colnames(benchmarks) <- c("expr","min", "lq", "mean", "median", "uq", "max")
	
	
	
	title <- 
		switch(type,
					 "double" = "tcrossprod Double Performance",
					 "float" = "tcrossprod Single Performance",
					 stop("Unrecongnized type")
		)
	
	for(i in 1:length(ORDER)){
		
		A <- getMatrix(ORDER[i])
		gpuA <- gpuMatrix(A, type = type)
		vclA <- vclMatrix(A, type = type)
		
		mbm <- microbenchmark(base = tcrossprod(A),
													gpu = tcrossprod(gpuA),
													vcl = vcl_tcrossprod(vclA),
													times = N, unit = "ms")
		
		benchmarks[[i]] <- mbm
	}
	
	df <- bind_rows(lapply(seq_along(benchmarks), function(i) { 
		benches <- summary(benchmarks[[i]])[,1:8]
		benches$expr <- as.character(benches$expr)
		cbind.data.frame(ORDER = as.character(rep(names(benchmarks)[i], 3)), benches, stringsAsFactors = FALSE) 
	}))
	
	df$ORDER <- as.numeric(df$ORDER)
	
	# plot
	p <- ggplot(df, aes(y = mean, x = ORDER, group = expr, colour=expr)) + 
		geom_line() + 
		geom_errorbar(aes(ymin = lq, ymax = uq)) +
		ggtitle(title) + 
		scale_y_continuous(name = "time (sec)")
	# scale_y_log10(name = "time (sec)")
	
	plot(p)
	return(df)
}