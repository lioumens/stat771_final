# Grid Size vs Time
# setwd("~/Desktop/s18/stat771/finalproject/src/")
source(file = "my_density_estimation.R") # for algorithms
source(file="timing_algs.R") # for timing function

pow2 <- 2^(4:11) # Density rounds up to powers of 2 for FFT

# Container for run times
result <- NULL
for (gs in pow2) {
  result <- rbind(result, timealgs(10, gs))
}

# Display with on the LOG scale 
matplot(log(result[2:7,]), type = "l", lwd=2, col=rainbow(3), lty=c(1,2,4), main="Log Run Time with increasing Gridsize", ylab="log(time (s))", xlab="", xaxt="n")
grid()
legend("topleft", fill=rainbow(3), legend=c("brute_kde", "fft_kde", "density"))
