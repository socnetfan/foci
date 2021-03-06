
setwd('D:/Dropbox/dhachen/NetSense/2mode')

library(RSiena)

rm(list=ls())
# read the data:
load('netsense.Rdata')

# add effects for model specification:
mydata <- mydata_music


# add effects for model specification:
myeff <- getEffects(mydata)
myeff <- includeEffects(myeff, transTrip, transRecTrip, cycle3, inPop, inInAss, name="net")
myeff <- includeEffects(myeff, X, name="net", interaction1 = "dorm")
myeff <- includeEffects(myeff, egoX, altX, simX,  name="net", interaction1 = "female")
myeff <- includeEffects(myeff, egoX, altX, simX,  name="net", interaction1 = "white")
myeff <- includeEffects(myeff, egoX, altX, simX,  name="net", interaction1 = "polview")
myeff <- includeEffects(myeff, from, name="net", interaction1="music")
myeff <- includeEffects(myeff, cycle4, name="music")
myeff <- includeEffects(myeff, outAct, name="music")
myeff <- includeEffects(myeff, inPop, name="music")
myeff <- setEffect(myeff, outInAss, name="music", parameter=1)
myeff <- includeEffects(myeff, to, name="music", interaction1="net")
myeff <- includeEffects(myeff, sharedTo, name="music", interaction1="net")
myeff <- setEffect(myeff, outActIntn, name="music", interaction1="net", parameter=1)
myeff <- includeEffects(myeff, egoX, name="music", interaction1="female")
myeff <- includeEffects(myeff, egoX, name="music", interaction1="white")
myeff <- includeEffects(myeff, egoX, name="music", interaction1="polview")
myeff

# set model name:
mymodel <- sienaAlgorithmCreate(projname='twomode-musics2')

# estimate model:
ans <- siena07(mymodel, data=mydata, effects=myeff, returnDeps=TRUE, batch=T)
ans
save(ans,file='twomode-musics2.Rdata')
while (summary(ans)$tconv.max[1,1] >=0.25 | max(abs(ans$tconv))>=0.1) {
  ans <- siena07(mymodel, data=mydata, effects=myeff, prevAns=ans, returnDeps=TRUE, batch=T)
  save(ans,file='twomode-musics2.Rdata')
} 
ans
# export results
z <- length(ans$theta)
m <- c(1:z)
n <- ans$effects[,2]
o <- round(ans$theta,4)
p <- rep(0,z)
for (i in 1:z) {
  p[i] <- round(sqrt(ans$covtheta[i,i]),4)
}
q <- round(2*pnorm(-abs(o/p)),4)
r <- round(ans$tconv,4)
final <- matrix(cbind(m,n,o,p,q,r),ncol=6)
write.table(final, "ans.csv", row.names=F, col.names=F, sep=",")


write.table(final, "twomode-musics2.csv", row.names=F, col.names=F, sep=",")
rm(list=ls())
















