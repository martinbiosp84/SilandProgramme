plotsiland.land<-function(x,land, data, var=0,lw=100)
{
  
  
  xr=range(land[[1]][,"X"])
  yr=range(land[[1]][,"Y"])
  lx=xr[2]-xr[1]
  ly=yr[2]-yr[1]
  wd=max(lx/lw,ly/lw)
  
  s.xr=seq(xr[1],xr[2],by=wd)
  s.yr=seq(yr[1],yr[2],by=wd)
  
  mgrid=expand.grid(s.xr,s.yr)
  colnames(mgrid)<-c("X","Y")
  nland=length(land)
  nl=length(x$coefficients)
  dres=x$coefficients[(nl-nland+1):(nl)]
  print("Distance computing... Wait...")
  Dist=calcdist(mgrid,land)
  print("Contribution computing... Wait..")
  landcontri=calcscontri(distmoy=dres,Distobs=Dist,sif=x$sif,w=wd)
  #landocntri is multiplied by the strength of the 
  #different landscape variables
  coef=x$coefficients[(nl-2*nland+1):(nl-nland)]
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
    sumpcontri=apply(pcontri,1,sum)
    
    matcontri=matrix(sumpcontri,nrow=length(s.xr))
    image.plot(s.xr,s.yr,matcontri,breaks=brks,col=colorTable,xlab="X",ylab="Y")
  }
    else
    {
     tmp=pcontri[,var] 
     matcontri=matrix(tmp,nrow=length(s.xr))
     image.plot(s.xr,s.yr,matcontri,breaks=brks,col=colorTable,xlab="X",ylab="Y")
     points(land[[var]][,c("X",("Y"))],pch=".",cex=1.2)
      
    }
    
invisible(list(x=s.xr,y=s.yr,mat=matcontri))
}

