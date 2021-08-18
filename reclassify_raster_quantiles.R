# takes an input raster x and a number of quantiles n
# and returns a reclassified raster r
# package "raster" needed

recl_quantiles <- function(x, n){
  q <- as.data.frame(raster::quantile(x))
  if(n == 2) {
    m <- matrix(c(0, q[3,1], 1, q[3,1], q[5,1], 2),
                           ncol = 3, byrow = TRUE)
  }
  if(n == 3) {
    m <- matrix(c(0, q[3,1], 1, q[3,1], q[4,1], 2,
                q[4,1], q[5,1], 3), ncol = 3, byrow = TRUE)
  }
  if(n == 4) {
    m <- matrix(c(0, q[2,1], 1, q[2,1], q[3,1], 2,
                q[3,1], q[4,1], 3, q[4,1], q[5,1], 4),
                ncol = 3, byrow = TRUE)
  }
  if(n == 5) {
    m <- matrix(c(0, q[1,1], 1, q[1,1], q[2,1], 2,
                q[2,1], q[3,1], 3, q[3,1], q[4,1], 4,
                q[4,1], q[5,1], 5), ncol = 3, byrow = TRUE)
  }
  r <- reclassify(x, m)
  return(r)
}
