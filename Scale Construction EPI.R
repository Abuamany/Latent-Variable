library(psych)
data(epi)
attach(epi)
head(epi)
data <- na.omit(epi)
summary(data)
str(data)
Mydata<-data.frame(data)

dim(Mydata)
describe(Mydata[1:57])

head(Mydata)




f5<-fa(Mydata[1:57],5)
p5 <- principal(Mydata[1:57],5)
factor.congruence(f5,p5)
summary(f5)

ic <- iclust(Mydata[1:57])
summary(ic)

factor.congruence(list(f5,p5,ic))

fa.lookup(f5,epi.dictionary)
fa.lookup(ic,epi.dictionary)

fa.diagram(f5)

p5p <-principal(Mydata[1:57],5,n.obs = 2897,rotate="Promax")
p5p
om.h <- omega(Mydata[1:57],n.obs=2897,sl=FALSE)
op <- par(mfrow=c(1,1))
om <- omega(Mydata[1:57],n.obs=2897)

p5p <-principal(Mydata[1:57],5,n.obs = 2897,rotate="Varimax")
p5p
om.h <- omega(Mydata[1:57],n.obs=2857,sl=FALSE)
op <- par(mfrow=c(1,1))
om <- omega(Mydata[1:57],n.obs=2857)
