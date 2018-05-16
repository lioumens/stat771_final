# -------------------------------------
# Compare density estimation algorithms
# 1. Density (from R)
# 2. my Brute force algorithm
# 3. my FFT algorithm
# -------------------------------------

# change working directory to the R files
# setwd("~/Desktop/gradwisc/s18/stat771/finalproject/src/")
source("my_density_estimation.R")

# Use Old faithful data
x <- faithful[[1]]

# Discretize
gridded <- gridify(x, 200)

# Create plots for comparison of algorithms
par(lwd=2)
plot(0, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
plot(density(x, bw=.05), type='l', main="Kernel Density Estimation Algorithms (bw=.05)", xlab="Waiting Time", col="chartreuse1")
grid(col="gray85", lwd=2)
lines(gridded$mesh, fft_kde(gridded$meshcount,gridded$ndata, .05), type="l", col="dodgerblue")
lines(gridded$mesh, brute_kde(gridded$mesh, gridded$meshcount, .05, gridded$ndata), type="l", col="chocolate1")
legend("topright", fill = c("chartreuse1","dodgerblue", "chocolate1"), legend = c("density", "fft_kde", "brute_kde")) 