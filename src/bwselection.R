# Bandwidth Selection
x <- faithful[[1]]

# Types of bandwidth
bws <- c("nrd0","SJ","ucv", "bcv")

par(mar=c(4, 4, 2, 2))

# Background of histogram
hist(x,breaks=30, freq=FALSE, xlim=c(1,5.5), ylim=c(0,.9), xlab="time", main="Old Faithful Eruption Times", ylab="density")
grid(col = "grey90", lwd=2) # I think the grid looks nice
for (i in 1:length(bws)) {
  d <- density(x, bw=bws[i])
  xgrid <- d$x
  ygrid <- d$y
  lines(xgrid, ygrid, col=rainbow(4)[i]) # Add a line for each bandwidth estimator
}

# See ?legend for explanation of parameters
legend("topright",legend = c("Silverman's Rule of Thumb", "Sheather Jones", "unbiased CV", "biased CV"), fill=rainbow(4),
       cex=.8, y.intersp = .9, text.width = 1)

