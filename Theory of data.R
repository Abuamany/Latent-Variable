library(psych)
set.seed(42)
p <-seq(-1.5, 1.5, 0.2)
n <- length(p)
pdif <- -p %+% t( p )
prob <- 1/(1 +exp(pdif))
match <-matrix(rbinom(n*n,1,prob),n,n)
games<-match
diag(games) <- NA
games

score <- rowMeans(games, na.rm = TRUE)
score
qscore <- qnorm(score) #convert means to normal deviates
logi <- logit(score)
#convert means to logit units
chess.df <- data.frame(latent = p, observed = score,normed = qscore, logi)
chess.df
#show the data
pairs.panels(chess.df) #plot the results in a SPLOM

score
btl <- score/(score %+% t(score))
round(btl,2)

resid <- games - btl
round(resid,2)

btl <- score/(score %+% t(score))

resid <- games - btl
sum(resid^2,na.rm=TRUE)
sum(games^2,na.rm=TRUE)
GF <- 1 -sum(resid^2,na.rm=TRUE)/sum(games^2,na.rm=TRUE)
GF

tests <- c("choice","logit","normal")
fits <- matrix(NA, ncol = 3,nrow=4)
for (i in 1:4) {for (j in 1:3) {fits[i, j] <- scaling.fits(chess.df[i], data = games,test = tests[j],rowwise=FALSE)$GF[1]} }
rownames(fits) <- c("latent","observed", "normed", "logistic")
colnames(fits) <- c("choice", "logistic", "normal")
round(fits, 2)




veg
colMeans(veg)
round(colMeans(veg),2)
p<- par(mfrow=c(1,2)) #I want to draw two graphs
plot(colMeans(veg))#the basic plot command
dotchart(colMeans(veg)) #dot charts are more informative
op <- par(mfrow=c(1,1)) #set the plotting back to a single graph
error.bars(veg,bars=TRUE,ylab="Preference",xlab="Vegetables",main="Mean and 95% confidence intervals")
round(veg,2)
round(colMeans(veg),2)
veg.t <- colMeans(veg) - mean(veg[,1])
round(veg.t,2)
z.veg <- qnorm(as.matrix(veg))
round(z.veg,2)
scaled.veg <- colMeans(z.veg)
round(scaled.veg,2)
scaled <- scaled.veg - min(scaled.veg)
round(scaled,2)
pdif <- - scaled %+% t(scaled)
colnames(pdif) <- rownames(pdif) <- colnames(z.veg)
round(pdif,2)
modeled <- pnorm(pdif)
round(modeled,2)
resid <- veg - modeled
round(resid,2)
sum(resid)
sum(resid^2)
sum(resid^2)/sum(veg^2)
1-sum(resid^2)/sum(veg^2)


cities
city.location <- cmdscale(cities, k=2)
plot(city.location,type="n", xlab="Dimension 1", ylab="Dimension 2",main ="cmdscale(cities)")
text(city.location,labels=names(cities))
round(city.location,0)

guttman <- matrix(rep(0,56),nrow=8)
for (i in 1:7) { for (j in 1:i) {guttman[i+1,j] <- 1}}
rownames(guttman) <- paste("S",1:8,sep="")
colnames(guttman) <- paste("O",1:7,sep="")
guttman
rowSums(guttman)
