plotsiland.land<-function(res,land, data, var=0,mwd=2)
{
  #size of pixel is multiplied by 2 to reduce
  #time computing
  w=mwd*min(dist(land[[1]][1:5,c("X","Y")]))
  
  xr=range(land[[1]][,"X"])
  yr=range(land[[1]][,"Y"])
  s.xr=seq(xr[1],xr[2],by=w)
  s.yr=seq(yr[1],yr[2],by=w)
  mgrid=expand.grid(s.xr,s.yr)
  colnames(mgrid)<-c("X","Y")
  nland=length(land)
  nl=length(res$coefficients)
  dres=res$coefficients[(nl-nland+1):(nl)]
  print("Distance computing... Wait...")
  Dist=calcdist(mgrid,land)
  print("Contribution computing... Wait..")
  landcontri=calcscontri(distmoy=dres,Distobs=Dist,sif=res$sif,w=w)
  #landocntri is multiplied by the strength of the 
  #different landscape variables
  coef=res$coefficients[(nl-2*nland+1):(nl-nland)]
  nr=nrow(landcontri)
  mcoef= matrix(rep(coef,nr),nr,byrow=T)
  pcontri=mcoef*landcontri
  #pcontri=landcontri
  sumpcontri=apply(pcontri,1,sum)
  mmax=max(c(abs(pcontri),abs(sumpcontri)))
  
  colorTable<- designer.colors(60, c( "darkblue","blue","white","red" ,"darkred") )
  #colorTable<- tim.colors(n=60,middle="white")
  brks<- seq(-mmax,mmax,length=61)
  
  if(var==0)
  {
  #  tmp=apply(pcontri,1,sum)
    matcontri=matrix(sumpcontri,nrow=length(s.xr))
    image.plot(s.xr,s.yr,matcontri,breaks=brks,col=colorTable,xlab="X",ylab="Y")
  }
    else
    {
     tmp=pcontri[,var] 
     matcontri=matrix(tmp,nrow=length(s.xr))
     image.plot(s.xr,s.yr,matcontri,breaks=brks,col=colorTable,xlab="X",ylab="Y")
      
    }
    
invisible(list(x=s.xr,y=s.yr,mat=matcontri))
}

