pkgname <- "siland"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('siland')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("dataSiland")
### * dataSiland

flush(stderr()); flush(stdout())

### Name: dataSiland
### Title: A simulated data set that reprsents observations.
### Aliases: dataSiland
### Keywords: datasets

### ** Examples

data(dataSiland)
data(landSiland)
nrow(dataSiland)
#Plot for landscape variables
plot(landSiland[[1]],col=2,pch=".")
points(landSiland[[2]],col=3,pch=".")
#Locations of observations
points(dataSiland[,c("X","Y")])






cleanEx()
nameEx("landSiland")
### * landSiland

flush(stderr()); flush(stdout())

### Name: landSiland
### Title: A list of simulated data sets that describes landscape.
### Aliases: landSiland
### Keywords: datasets

### ** Examples

data(landSiland)
names(landSiland)
#locations for the two landscape variables b1 and b2
plot(landSiland[[1]],col=2,pch=".")
points(landSiland[[2]],col=3,pch=".")



cleanEx()
nameEx("plotcontri")
### * plotcontri

flush(stderr()); flush(stdout())

### Name: plotcontri
### Title: Plot contributions
### Aliases: plotcontri

### ** Examples

data(landSiland)
data(dataSiland)
res=siland(y~locvar,land=landSiland,data=dataSiland,sif="exponential",family=gaussian)
plotcontri(res,landSiland,dataSiland,type=1)
plotcontri(res,landSiland,dataSiland,numvar=2)



cleanEx()
nameEx("plotsif")
### * plotsif

flush(stderr()); flush(stdout())

### Name: plotsif
### Title: Plot density and cumultive density for sif function
### Aliases: plotsif

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.



cleanEx()
nameEx("plotsiland")
### * plotsiland

flush(stderr()); flush(stdout())

### Name: plotsiland
### Title: Plot results from siland function
### Aliases: plotsiland

### ** Examples




cleanEx()
nameEx("plotsiland.land")
### * plotsiland.land

flush(stderr()); flush(stdout())

### Name: plotsiland.land
### Title: Spatial representation of the landscape influence
### Aliases: plotsiland.land
### Keywords: ~kwd1 ~kwd2

### ** Examples




cleanEx()
nameEx("plotsiland.sif")
### * plotsiland.sif

flush(stderr()); flush(stdout())

### Name: plotsiland.sif
### Title: Plot the estimated spatial influence functions.
### Aliases: plotsiland.sif
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.




cleanEx()
nameEx("siland")
### * siland

flush(stderr()); flush(stdout())

### Name: siland
### Title: Estimation of spatial influence of landscape
### Aliases: siland

### ** Examples





cleanEx()
nameEx("siland.quantile")
### * siland.quantile

flush(stderr()); flush(stdout())

### Name: siland.quantile
### Title: Quantile computation for spatial influence functions
### Aliases: siland.quantile

### ** Examples





### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
