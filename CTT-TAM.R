#Load the TAM library
library("TAM")

#set working directory and read data file

raw_resp <- read.csv("D1_resp.csv")

#score responses
key <- c(1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1)
scored <- sapply( seq(1,length(key)), FUN = function(ii){ 1*(raw_resp[,ii] == key[ii]) } )

#run IRT analysis
mod1 <- tam(scored)

#Ability estimate - Weighted Likelihood Estimate
Abil <- tam.wle(mod1)

#CTT statistics
ctt1 <- tam.ctt(raw_resp, Abil$theta)
write.csv(ctt1,"D1_ctt1.csv")

#Fit statistics
Fit <- tam.fit(mod1)
Fit

#Plot expected scored curves.
plot(mod1)
