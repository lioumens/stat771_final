# Bandwidth figure, shows effect of bandwidth with guassian kernel

# First plot, 
par(mfrow=c(1,2), mar=c(4,4,2,2))
set.seed(6)
n <- 5
h <- .6 # bandwidth
X <- rnorm(n)
x <- seq(-4, 4, .01)
fhat <- rep(0, length(x))
plot(0,0,type="n", xlab = 'x', ylab = 'density',
     xlim = c(-3.5, 3.5), ylim = c(0, .6),
     main = 'KDE (Guassian, h = .6)')

for(i in 1:n){
  f <- dnorm((x - X[i]) / h) * (1/(n * h))
  lines(x, f, col = 2, lty=2) # show each guassian distribution
  fhat <- fhat + f
}
lines(x, fhat, lwd=3) # show overall density estimate
points(X, rep(-.01, n), pch=17, cex=2, col=4) # Show centers of gaussians

# Second plot
h <- .25 # Change the bandwidth
fhat <- rep(0, length(x))
plot(0,0,type="n", xlab = 'x', ylab = 'density',
     xlim = c(-3.5, 3.5), ylim = c(0, .6),
     main = 'KDE (Guassian, h = .25)')

for(i in 1:n){
  f <- dnorm((x - X[i]) / h) * (1/(n * h))
  lines(x, f, col = 2, lty=2) # each gaussian distribution
  fhat <- fhat + f
}
lines(x, fhat, lwd=3) # overall estimate
points(X, rep(-.01, n), pch=17, cex=2, col=4)