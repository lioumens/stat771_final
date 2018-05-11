# Amount of Data vs AMISE approximated by cross validation

set.seed(10)
par(mfrow=c(1,1), lwd=2)
# faithful data
x <- faithful[[1]]

ndata <- seq(10,length(x), by=10)

#' Calculate errors
# Modified from https://www.r-bloggers.com/cross-validation-for-kernel-density-estimation/
risk_cv <- function(x){
  fhat=Vectorize(function(z) density(x,from=z,to=z,n=1)$y)
  fhati=Vectorize(function(z) density(x[-z],from=x[z],to=x[z],n=1)$y)
  F=fhati(1:length(x))
  return(integrate(function(x) fhat(x)^2, -Inf, Inf)$value - 2 * mean(F))
}

hist(x, breaks=25, freq=FALSE, type="n", xlim=c(1,5.5), ylim=c(0, .9), main="Better KDE with increasing data (n)")

result <- NULL

for (i in 1:length(ndata)) {
  xs <- sample(x, size = ndata[i], replace = FALSE)
  d <- density(xs, from=1, to=5.5)
  xgrid <- d$x
  ygrid <- d$y
  lines(xgrid, ygrid,col=rgb(1,0, 0, i/27), lwd=.3)
  result <- c(result,risk_cv(xs)) # store risks
}

# Calculate that risk follows lower bound.
nfourfifths <- function(n) {return(n^(-4/5))}

# Plot n^(-4/5) on this grid
g <- seq(0, 250)
gy <- nfourfifths(g)
plot(ndata, result, type="l", main="Silverman's Rule of Thumb follows Asymptotic LB trend", ylab="Est. Risk")
lines(g, gy - .4, col="red", lty=2) # shift the values down to the surrogate risk function
legend("topright", fill=1:2, legend=c("Cross-Validated Risk Estimate", "Asymptotic Lower Bound"))

# We can see that the asymptotic risk for the Silverman Rule of thumb follows the asymptotic lower bound really well!



