
cases <- 1:1000

print("EXP 1")
ptm <- proc.time()
   for (i in cases) {
     res<-DT[,mean(pwgtp15),by=SEX]
   }
print(proc.time() - ptm)


print("EXP 2")
ptm <- proc.time()
   for (i in cases) {
     mean(DT[DT$SEX==1,]$pwgtp15)
     mean(DT[DT$SEX==2,]$pwgtp15)
   }
print(proc.time() - ptm)

print("EXP 3")
ptm <- proc.time()
   for (i in cases) {
     mean(DT[DT$SEX==1,]$pwgtp15)
   }
print(proc.time() - ptm)

print("EXP 4")
ptm <- proc.time()
   for (i in cases) {
     sapply(split(DT$pwgtp15,DT$SEX),mean)
   }
print(proc.time() - ptm)

print("EXP 5")
ptm <- proc.time()
   for (i in cases) {
      tapply(DT$pwgtp15,DT$SEX,mean)
   }
print(proc.time() - ptm)

# print("EXP 6")
# system.time( {
#   rowMeans(DT)[DT$SEX==1]
#   rowMeans(DT)[DT$SEX==2]
# })

print("EXP 7")
ptm <- proc.time()
   for (i in cases) {
     mean(DT$pwgtp15,by=DT$SEX)
   }
print(proc.time() - ptm)


