# Testing Notes

Potential tests to add:

  * Add more species to confirm optimization procedure is working
  
  * check that normalization works consistently (or trivial?), particulalrly when reading in a compressed file
  
  * check that the compression doesn't lead to significant information loss (mse between uncompressed and untransformed compressed below some threshold?)
  
  * would be nice to check that fit buffer works right in a case where we know the needed buffer
  
  *compare grass and R based Rdist calculations to confirm. you can turn off the check on cran so you don't run things there that depend on grass.