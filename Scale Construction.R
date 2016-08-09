library(psych)
data(bfi)
attach(bfi)
head(bfi)
data <- na.omit(bfi)
summary(data)
str(data)
Mydata<-data.frame(data)

dim(Mydata)
describe(Mydata[1:25])

head(Mydata)




f5<-fa(Mydata[1:25],5)
p5 <- principal(Mydata[1:25],5)
factor.congruence(f5,p5)
summary(f5)

ic <- iclust(Mydata[1:25])
summary(ic)

factor.congruence(list(f5,p5,ic))

fa.lookup(f5,bfi.dictionary[2:3])
fa.lookup(ic,bfi.dictionary[2:3])

fa.diagram(f5)

p5p <-principal(Mydata[1:25],5,n.obs = 2236,rotate="Promax")
p5p
om.h <- omega(Mydata[1:25],n.obs=2236,sl=FALSE)
op <- par(mfrow=c(1,1))
om <- omega(Mydata[1:25],n.obs=2236)

p5p <-principal(Mydata[1:25],5,n.obs = 2236,rotate="Varimax")
p5p
om.h <- omega(Mydata[1:25],n.obs=2236,sl=FALSE)
op <- par(mfrow=c(1,1))
om <- omega(Mydata[1:25],n.obs=2236)
