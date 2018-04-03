plotsiland.land2<-function(theta,dsif,land,sif="exponential", data=NULL, var=0,mwd=2,...)
{
  #size of pixel is multiplied by 2 to reduce
  #time computing
  w=min(dist(land[[1]][1:5,c("X","Y")]))
  sizegrid=mwd*w
  
  xr=range(land[[1]][,"X"])
  yr=range(land[[1]][,"Y"])
  s.xr=seq(xr[1],xr[2],by=sizegrid)
  s.yr=seq(yr[1],yr[2],by=sizegrid)
  mgrid=expand.grid(s.xr,s.yr)
  colnames(mgrid)<-c("X","Y")
  nland=length(land)
  print("Distance computing... Wait...")
  Dist=calcdist(mgrid,land)
  print("Contribution computing... Wait..")
  landcontri=calcscontri(distmoy=dsif,Distobs=Dist,sif=sif,w=w)
  
  #landocntri is multiplied by the strength of the 
  #different landscape variables
  nr=nrow(landcontri)
  mcoef= matrix(rep(theta,nr),nr,byrow=T)
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
    image.plot(s.xr,s.yr,matcontri,breaks=brks,col=colorTable,xlab="X",ylab="Y",...)
    if(!is.null(data))
      points(data[,c("X","Y")],pch=16,cex=1.1)
  }
    else
    {
     tmp=pcontri[,var] 
     matcontri=matrix(tmp,nrow=length(s.xr))
     image.plot(s.xr,s.yr,matcontri,breaks=brks,col=colorTable,xlab="X",ylab="Y",...)
     if(!is.null(data))
       points(data[,c("X","Y")],pch=16,cex=1.1)
    }
    
invisible(list(x=s.xr,y=s.yr,mat=matcontri))
}



