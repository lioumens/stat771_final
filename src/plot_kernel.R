# Shows effect of different kernel selection

# Kernel Graph
par(mfrow=c(1,2), oma=c(1, 1, 1, 1), mar=c(1, 1, 1, 1), lwd=2)
kernels <- eval(formals(density.default)$kernel)
plot (density(0, bw = 1), xlab = "",
      main = "R's density kernels, h=1", xaxt="n", yaxt="n", ylim=c(0,.45))
grid(col="grey90")
for(i in 2:length(kernels))
  lines(density(0, bw = 1, kernel =  kernels[i]), col = i)

# Plot 2, show the kernel estimates
bw <- bw.SJ(unicef[,2])
h.f <- sapply(kernels, function(k)density(kernel = k, give.Rkern = TRUE))
h.f <- (h.f["gaussian"] / h.f)^ .2
plot(density(unicef[,2], bw = bw),
     main = "Life Expectancy, 7 kernels, h=3.42",
     xlab="years", xaxt="n", yaxt="n", ylim=c(0,.04))
grid(col="grey90")
for(i in 2:length(kernels))
  lines(density(unicef[,2], bw = bw, adjust = h.f[i], kernel = kernels[i]),
        col = i)

# create an empty overlay for the legend
par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")

legend("bottom", legend = kernels, col =seq(kernels),
       lty = 1, cex = 1, y.intersp =.3,x.intersp=.4,text.width = .18, ncol=7 , xpd=TRUE, inset=c(0,0), bty="n")