tca_s2 <- function(img){
  
  if ("B10" %in% names(s2_10)) {
    brightness <- 0.3037*img[["B2"]]+0.2793*img[["B3"]]+0.4343*img[["B4"]]+
      0.5585*img[["B8"]]+0.5082*img[["B10"]]+0.1863*img[["B12"]]
    
    greenness <- -0.2848*img[["B2"]]+(-0.2435*img[["B3"]])+(-0.5436*img[["B4"]])+
      0.7243*img[["B8"]]+0.0840*img[["B11"]]+(-0.1800*img[["B12"]])
    
    yellowness <- 0.1509*img[["B2"]]+0.1793*img[["B3"]]+0.3299*img[["B4"]]+
      0.3406*img[["B8"]]+(-0.7122*img[["B11"]])+(-0.4572*img[["B12"]])
    tc <- stack(brightness,greenness,yellowness)
  }
  
  else {
    greenness <- -0.2848*img[["B2"]]+(-0.2435*img[["B3"]])+(-0.5436*img[["B4"]])+
      0.7243*img[["B8"]]+0.0840*img[["B11"]]+(-0.1800*img[["B12"]])
    
    yellowness <- 0.1509*img[["B2"]]+0.1793*img[["B3"]]+0.3299*img[["B4"]]+
      0.3406*img[["B8"]]+(-0.7122*img[["B11"]])+(-0.4572*img[["B12"]])
    tc <- stack(greenness,yellowness)
  }
  
  return(tc)
  
}
