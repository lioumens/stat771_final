#' Takes continuous data and discretizes it over a grid with specified size.
#'
#' @param x A 1-d vector of data to discretize
#' @param gridsize Size of the grid over which to discretize the data x
#' @return 
#' @examples
#' gridify(faithful[[1]])
gridify <- function(x, gridsize=100){
  # Create Grid that extends a little past the data
  mesh <- seq.int(min(x) - 2*sd(x), max(x) + 2*sd(x), length.out=gridsize)
  meshwidth <- mesh[2]-mesh[1]
  
  # Container for data counts
  meshcount <- vector(length=gridsize, mode = "numeric")
  
  # Split data points over counts on the grid
  for (i in x){
    xpos <- (i - mesh[1])/meshwidth # position index of value
    li <- as.integer(ceiling(xpos)) # left index
    ri <- li + 1L # right index
    
    # If a point falls between two indices,
    # We split the count linearly by distance, eg, if a point falls between index4 and index5,
    # closer to 5, we would add .25 to index4 and .75 to index5
    # 4___._5
    meshcount[li] <- meshcount[li] + abs(xpos - li)
    meshcount[ri] <- meshcount[ri] + (1 - abs(xpos - li))
  }
  result <- structure(list(data=x,
                           ndata=length(x),
                           mesh=mesh,
                           meshcount=meshcount))
  return(result)
}


#' Manual method of density estimation
#'
#' @param mesh The grid of points over which data was discretized
#' @param meshcount Binned counts of the data on the mesh
#' @param ndata length of the data
#' @param bandwidth The standard deviation of the guassian kernel
#' @return Density estimates at each value of the grid
#' @examples
#' gridded <- gridify(faithful[[1]])
#' brute_kde(gridded$mesh, gridded$meshcount, .3, gridded$ndata)
brute_kde <- function(mesh, meshcount, bandwidth, ndata){
  gridsize <- length(mesh)
  # container for final estimates
  fhat <- vector(length = gridsize, mode = "numeric")
  
  # outer loop - estimates for each grid point
  # inner loop - sum density contribution from all other grid points
  for (i in 1:gridsize) {
    temp <- 0
    for (j in 1:gridsize) {
      temp <- temp + dnorm(mesh[i] - mesh[j], sd=bandwidth) * meshcount[j]
    }
    fhat[i] <- temp / ndata
  }
  return(fhat)
}

#' FFT method of density estimation
#'
#' @param meshcount Binned counts of the data on the mesh
#' @param ndata length of the data
#' @param bandwidth The standard deviation of the guassian kernel
#' @return Density estimates at each value of the grid
#' @examples
#' gridded <- gridify(faithful[[1]])
#' fft_kde(gridded$meshcount, gridded$ndata, .3)
fft_kde <- function(meshcount, ndata, bandwidth){
  gridsize <- length(meshcount)
  kmesh <- seq.int(0, 6, length.out = gridsize)
  kmesh[(gridsize/2 + 2):(2 * gridsize/2)] <- -kmesh[(gridsize/2):2]
  meshkern <- dnorm(kmesh, sd=bandwidth)
  fhat <- Re(fft(fft(meshcount / ndata) * Conj(fft(meshkern)), inverse=TRUE)) / ndata
  return(fhat)
}


# Variables for modification
# gridsize <- 200
# bw <- .3
# mesh <- seq(min(x) - 2*sd(x), max(x) + 2*sd(x), length.out=gridsize)
# meshwidth <- mesh[2] - mesh[1]
# meshcount <- vector(length=gridsize, mode = "numeric")

# Discretize the space
# for (i in x){
#   xpos <- (i - mesh[1])/meshwidth # position index of value
#   li <- as.integer(ceiling(xpos)) # left index
#   ri <- li + 1L # right index
#   # If a point falls between two indices,
#   # We split the count linearly by distance, eg, if a point falls between index4 and index5,
#   # closer to 5, we would add .25 to index4 and .75 to index5
#   # 4___o_5
#   meshcount[li] <- meshcount[li] + abs(xpos - li)
#   meshcount[ri] <- meshcount[ri] + (1 - abs(xpos - li))
# }
# 
# plot(density(x, bw=.2))
# lines(mesh, meshy)



# Manual method of desnity estimation
# outer loop - estimates for each grid point
# inner loop - sum density contribution from all other grid points
# fhat <- vector(length = gridsize, mode = "numeric") # save the values
# for (i in 1:gridsize) {
#   temp <- 0
#   for (j in 1:gridsize) {
#     temp <- temp + dnorm(mesh[i] - mesh[j], sd=bw) * meshcount[j]
#   }
#   fhat[i] <- temp / length(x)
# }
# plot(density(x))
# lines(mesh, fhat, type="l")

# THATS IT FOR MANUAL METHOD


# FFT Method
# assigning some linear weight that is the distance to each point on the mesh
# kords <- seq.int(0, 6, length.out = gridsize)
# kords[(gridsize/2 + 2):(2 * gridsize/2)] <- -kords[(gridsize/2):2]
# other <- dnorm(kords, sd=.2)
# meshy <- Re(fft(fft(meshcount / length(x)) * Conj(fft(other)), inverse=TRUE)) / length(x)
# plot(density(x, bw=.2))
# lines(mesh, meshy)
# fft(other)
# fft(meshcount)
# 
# 
# plot(mesh, meshy)
# density(x)$y
