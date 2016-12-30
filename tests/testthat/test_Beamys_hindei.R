## Test loads sample data for Beamys_hindei, fits a single expert map and confirms:
# 1) rangeOffset() is able to fit the curve to acheive 0.7 pinside
# 2) The calculation of pinside using the frequency table matches that from the full raster
# 3) The range size is a known percentage of the full domain

## Load data
data(Beamys_hindei_points)
data(Beamys_hindei_range)

## Create raster domain
domain=raster(xmn=-180, xmx=180, ymn=-90, ymx=90, 
              crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs", 
              resolution=.1, vals=NULL) #30/3600

## Calculate range dists
rdist=rangeDist(Beamys_hindei_range,domain=domain,domainkm=1000,mask=F,fact=2,verbose=F)    

## Create frequency table
dists=freq(rdist,useNA="no",digits=1)

## Set desired expert decay
vars=c(prob=0.7,rate=.1,skew=0.2,shift=0,buffer=0)

## Fit the decay curve for the desired expert decay
expert=rangeOffset(rdist,
                   dists=dists,
                   parms=vars,
                   normalize=T,
                   verbose=F,
                   writeRaster=F)

## Extract metadata
mt=metadata(expert)
names(mt$parms)=mt$pnames
mt$parms

context("Testing range offset optimization")

test_that("Desired p_in is achieved", {
  expect_equal(mt$parms["pinside"], vars["prob"], tolerance=1,check.attributes=F)
})


expert2=mask(expert,rdist<=0,maskval=0)


test_that("Calculation of pinside on full raster is within 1% of that from frequency table", {
  expect_equal(mt$parms["pinside"], cellStats(expert2,sum), tolerance=1,check.attributes=F)
})


test_that("Calculation of range size as percentage of full domain", {
  expect_equal(cellStats(rdist<=0,sum)/ncell(rdist), 0.033, tolerance=0.1,check.attributes=F)
  })
