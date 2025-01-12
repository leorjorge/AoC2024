d <- read.table("InputDay1.txt", header = F)
d <- as.data.frame(sapply(d, sort))
d[,3] <- abs(d[,1]-d[,2])   
answer1 <- sum(d[,3])

Count2 <- table(d$V2)

for (i in 1:nrow(d)){
   if(d$V1[i] %in% names(Count2)){
      d[i,4] <- Count2[names(Count2) == d$V1[i]] * d$V1[i]
   } else{
      d[i,4] <- 0
   }
}
sum(d$V4)
