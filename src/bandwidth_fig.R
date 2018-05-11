# Bandwidth figure
par(mfrow=c(1,2), mar=c(4,4,2,2))
set.seed(6)
n <- 5
h <- .6
X <- rnorm(n)
x <- seq(-4, 4, .01)
fhat <- rep(0, length(x))
plot(0,0,type="n", xlab = 'x', ylab = 'density',
     xlim = c(-3.5, 3.5), ylim = c(0, .6),
     main = 'KDE (Guassian, h = .6)')

for(i in 1:n){
  f <- dnorm((x - X[i]) / h) * (1/(n * h))
  lines(x, f, col = 2, lty=2)
  fhat <- fhat + f
}
lines(x, fhat, lwd=3)
points(X, rep(-.01, n), pch=17, cex=2, col=4)

h <- .25
fhat <- rep(0, length(x))
plot(0,0,type="n", xlab = 'x', ylab = 'density',
     xlim = c(-3.5, 3.5), ylim = c(0, .6),
     main = 'KDE (Guassian, h = .25)')

for(i in 1:n){
  f <- dnorm((x - X[i]) / h) * (1/(n * h))
  lines(x, f, col = 2, lty=2)
  fhat <- fhat + f
}
lines(x, fhat, lwd=3)
points(X, rep(-.01, n), pch=17, cex=2, col=4)