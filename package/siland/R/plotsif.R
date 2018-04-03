plotsif<-function(d=NULL,sif="exponential" )
{
if(is.null(d))
  stop("argument d has to be numeric value")
#if((sif!="exponential") || (sif!="gaussian"))
#  stop("argument sif has to be \"gaussian\" or \"exponential\" ")
x=seq(0,4*d,length=200)
ua=x[2]
if(sif=="exponential")
{
  y=fdispE(x,d)
  yr=fdispRE(x,d)
}
if(sif=="gaussian")
{
  y=fdispG(x,d)
  yr=fdispRG(x,d)
}

par(mfrow=c(1,2))
plot(x,y,type="l",xlab="distance from source",ylab="density")
cyr=cumsum(yr)*ua
plot(x,cyr,type="l",xlab="radius around source",ylab="cumulative density")


return(list(density=cbind(x=x,y=y),cumdensity=cbind(x=x,y=cyr)))
}
