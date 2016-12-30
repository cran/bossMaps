## Test loads sample expert data for Leucadendron lanigerum and calculates the distance to range function and confirms expected min/max distances.


## Load data
data("Leucadendron_lanigerum_range")


range=Leucadendron_lanigerum_range

rdist=rangeDist(range,domain=range,domainkm=2500,mask=F,fact=2, verbose=FALSE)    

context("Testing distance calculation using a rasterized range as input")

test_that("Check min/max values for Leucadendron lanigerum range distances", {
  expect_equal(rdist@data@min,-19.38,tolerance=0.01)
  expect_equal(rdist@data@max,496.88,tolerance=0.01)
})

