\name{dataCmoth}
\alias{dataCmoth}
\docType{data}
\title{
Data set for codling moth counts
}
\description{
The dataframe dataCmoth gives measurements for codling moth 
counts in 54 orchards. X and Y give the locations for observed data. The number Cmoth is the count of collected
 larvae in cardborad traps wrapped around tree trunks. trait is a local variable that gives the number of traitements for codling moth in the orchard. orchard gives the orchard number.

}
\usage{data("dataCmoth")}
\format{
  A data frame with 1574 observations on the following 5 variables.
  \describe{
    \item{\code{X}}{a numeric vector}
    \item{\code{Y}}{a numeric vector}
    \item{\code{Cmoth}}{a numeric vector}
    \item{\code{trait}}{a numeric vector}
    \item{\code{orchard}}{a factor with levels \code{10} \code{104} \code{11} \code{114} \code{117} \code{119} \code{121} \code{122} \code{123} \code{124} \code{125} \code{126} \code{129} \code{13} \code{130} \code{131} \code{132} \code{134} \code{135} \code{138} \code{139} \code{140} \code{141} \code{145} \code{148} \code{149} \code{150} \code{152} \code{153} \code{154} \code{157} \code{159} \code{17} \code{18} \code{26} \code{28} \code{35} \code{36} \code{4} \code{41} \code{42} \code{51} \code{54} \code{55} \code{62} \code{65} \code{68} \code{7} \code{71} \code{75} \code{76} \code{84} \code{88} \code{95}}
  }
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
\keyword{datasets}
