

toydata<-function(n=200,corte=0.99,expo1=2,expo2=4,a=1,b=-3,c=18,size=1,semilla=12345)
{
  library(ggplot2)
  set.seed(semilla)
  f<-data.frame()
  for (i in seq(1,n,by=1))
  {
    x1 = (rnorm(1))
    x2 = (rnorm(1))
    cosa=rnorm(1)
    z = a*(x1**expo1) + b*(x2**expo2)+c*cosa
    pr = 1/(1+exp(-z))
    clase =ifelse(pr > corte,1,0)
    #now feed it to glm:
    df = data.frame(clase=factor(clase),x1=x1,x2=x2)
    f<-rbind(f,df)
  }
  liscol<-c("darkolivegreen3", "red")
  colScale <- scale_colour_manual(name = "vardep",values = liscol)
  a<-ggplot(f, aes(x=x1, y=x2,col=clase)) + geom_point(size=size)
  a<-a+colScale
  return(list(a,f))
}

# v<-toydata(n=400,0.7,2,4,1,-3,0.5)
#
# v[[1]]
# ve<-v[[2]]

spiral<-function(width=20,height=20,n1=200,n2=200,rbis=3,dtip=1,size=1)
{
  library(ggplot2)
  f<-data.frame()
  g<-data.frame()
  n=0
  r=0

  for (i in seq(0,n1,by=1))
  {

    x1<-width/2 + cos(n) * r+(rnorm(1))*dtip
    x2<-height/2 + sin(n) * r+(rnorm(1))*dtip
    a<-data.frame(x1,x2)
    f<-rbind(f,a)
    n=n+0.05
    r=r+0.1
  }
  f$clase="1"
  n=0
  r=rbis
  for (i in seq(0,n2,by=1))
  {

    x1<-width/2 + cos(n) * r+(rnorm(1))*dtip
    x2<-height/2 + sin(n) * r+(rnorm(1))*dtip
    a<-data.frame(x1,x2)
    g<-rbind(g,a)
    n=n+0.05
    r=r+0.1
  }
  g$clase<-"0"
  un<-rbind(f,g)
  liscol<-c("darkolivegreen3", "red")
  colScale <- scale_colour_manual(name = "vardep",values = liscol)
  a<-ggplot(un, aes(x=x1, y=x2,col=clase))+geom_point(size=size) +colScale

  return(list(a,un))
}


# sp<-spiral(width=20,height=20,n1=400,n2=401,rbis=6,dtip=1.5)
#
# sp[1]
# archivo<-sp[[2]]
