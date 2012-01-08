\name{fundist}
\title{fundist =function to calculate the median of the distribution of pigment stages}
\usage{
  fundist(Vparm, phicum, graph = TRUE, lmax = 1)
}
\arguments{
  \item{lmax}{scale parameterof the graphical function see fnstades}

  \item{graph}{logical, to see the curves type
  graph = TRUE}
}
\note{
  pigment stage functions are not standard statistical distribution,
 calculating where 50\% of the distribution lies is done with fun50
  this function uses \link{fnstade} to calculate the values  
  of pigment times on a regular scale (phicum)
}
\examples{

 Vparm<-list()
# below param for briand et al.,2005 pigmentation function in glass eel
Vparm$pigment_stage$p1<-0.267 # parameters for gamma functions describing changes from stage to stage
Vparm$pigment_stage$p2<-0.835
Vparm$pigment_stage$p3<-1.56
Vparm$pigment_stage$p4<-3.682
Vparm$pigmentation$teta<- 30 # bounding parameters for beta function
Vparm$pigmentation$sigma<-40 # bounding parameters for beta function
Vparm$pigmentation$sigma2<--5 # bounding parameters for beta function
Vparm$pigmentation$p5<- 4.566 # param for beta function
Vparm$pigmentation$p6<-8.141
Vparm$pigmentation$p7<-0.071 # param for beta function
Vparm$pigmentation$p8<-0.426
fundist(Vparm,seq(0,10, length.out=10000),graph=FALSE,lmax=1)
fundist(Vparm,seq(0,10, length.out=10000),graph=TRUE,lmax=1)
fundist(Vparm,seq(0,10, length.out=10000),graph=TRUE,lmax=0)
plot(seq(0,10, length.out=10000),pgamma(seq(0,10, length.out=10000),Vparm$pigment_stage[[1]]),col="pink")
points(seq(0,10, length.out=10000),pgamma(seq(0,10, length.out=10000),Vparm$pigment_stage[[2]]),col="firebrick") 
}
\seealso{
	\code{\linkS4class{Bilan_stades_pigm}}	
}