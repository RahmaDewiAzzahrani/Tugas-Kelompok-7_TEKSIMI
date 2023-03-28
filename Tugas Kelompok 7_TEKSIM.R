#KELOMPOK_7
#Rahma Dewi AZZAHRANI_B2A020007
#Ammil Nur Abdullah_B2A020013
#Elvia Nanda Sofiyanti_B2A020033

multiplicative_RNG<-function(a,z0,m,n) {
  xj<-matrix(NA,n,3)
  colnames(xj)<-c("aZ","Xj","Uj")
  for (j in 1:n)
  {
    xj[j,1]<-(a*z0)
    xj[j,2]<-xj[j,1]%%m
    xj[j,3]<-xj[j,2]/m
    z0<-xj[j,2]
  }
  hist(xj[,3])
  View(xj)
  i<-xj[,3]
  p<-0.5
  R<-runif(i)
  X<-log(1-R)/log(1-p)
  hist(X)
  X
}