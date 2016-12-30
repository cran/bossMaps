library(raster)
library(devtools)

species="Beamys hindei"

  dsp=MOLget(species,type=c("points","range"))
  Beamys_hindei_points=dsp[["points"]]
  Beamys_hindei_range=dsp[["range"]]
  
  #save it
  use_data(Beamys_hindei_points)
  use_data(Beamys_hindei_range)
  
  mt=metadata(dt)
  names(mt$parms)=mt$pnames
  
  
### Load example expert prior raster
  Leucadendron_lanigerum_range=readAll(raster("inst/extdata/Leucadendron_lanigerum.tif"))
  use_data(Leucadendron_lanigerum_range,overwrite=T)
  
