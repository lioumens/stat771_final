# Timing Algorithms
setwd("~/Desktop/s18/stat771/finalproject/src/")
source(file = "my_density_estimation.R")

#' Time the three algorithms: brute_kde, fft_kde, density
#' Averaged over a navg trials, and mean is reported
#'
#'@param navg Number of times to average the algorithm over
#'@param gridsize What should the gridsize be? (default=512)
timealgs <- function(navg, gridsize=512) {
  result <- NULL
  for (i in 1:navg){
    times <- c(0, 0, 0)
    start <- Sys.time()
    gridded <- gridify(x, gridsize)
    fft_kde(gridded$meshcount,gridded$ndata, .05)
    end <- Sys.time()
    times[2] <- end - start
    
    start <- Sys.time()
    gridded <- gridify(x, gridsize)
    brute_kde(gridded$mesh, gridded$meshcount, .05, gridded$ndata)
    end <- Sys.time()
    times[1] <-  end - start
    
    start <- Sys.time()
    density(x, n=gridsize)
    end <- Sys.time()
    times[3] <-  end - start
    
    # Save results
    result <- rbind(result,times)
  }
  algtimes <- colMeans(result)
  names(algtimes) <- c("brute", "fft", "density")
  return(algtimes)
}

print(timealgs(10, 512))