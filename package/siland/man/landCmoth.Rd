\name{landCmoth}
\alias{landCmoth}
\docType{data}
\title{
Data sets for landscape description: organic and conventional features.
}
\description{
landCmoth is a list with two components. The first one gives the location of pixels for 
organic variable and the second one for conventional variable.
}
\usage{data("landCmoth")}
\format{
  The format is:
List of 2
 $ organic     :'data.frame':	36 obs. of  3 variables:
  ..$ X  : num [1:36] 855915 855628 855628 855628 855724 ...
  ..$ Y  : num [1:36] 6307065 6303534 6303431 6303327 6303327 ...
  ..$ Bio: num [1:36] 1 1 1 1 1 1 1 1 1 1 ...
 $ conventional:'data.frame':	1099 obs. of  3 variables:
  ..$ X   : num [1:1099] 852570 852952 853048 853239 853334 ...
  ..$ Y   : num [1:1099] 6307376 6307376 6307376 6307376 6307376 ...
  ..$ Conv: num [1:1099] 1 1 1 1 1 1 1 1 1 1 ...
}

\references{
Ricci B., Franck P., Bouvier J.-C., Casado D. and Lavigne C. (2011). Effects of hedgerow characteristics on intra-orchard distribution of larval codling moth. Agriculture, Ecosystems & Environment. 140. 395-400. 10.1016/j.agee.2011.01.001. 
}



\examples{
\donttest{
data(landCmoth)
data(dataCmoth)
#conventional pixels
plot(landCmoth[[2]][,c("X","Y")],pch=16,cex=0.3,col=2)
#organic pixels
points(landCmoth[[1]][,c("X","Y")],pch=16,cex=0.3,col=3 )
#measurement locations
points(dataCmoth[,c("X","Y")],pch=4,cex=0.8,col=1)
}
}

