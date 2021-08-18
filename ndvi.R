ndvi <- function(x, r, nir) {
  
  # x defines the raster (brick or stack) which is needed to calculate the ndvi
  # r and nir need to specify the band names, in quotations
  
  if(class(x)[1] == "RasterBrick" | 
    class(x)[1] == "RasterStack") {
    ndvi <- (x[[r]] - x[[nir]]) / (x[[r]] + x[[nir]])
  }
  else {
    print("wrong input")
  }
}
