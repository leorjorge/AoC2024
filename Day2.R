d <- readLines("InputDay2.txt") |> 
   as.list() |> 
   lapply(FUN = function(x){
      as.numeric(unlist(strsplit(x = x, split = " ")))
   }) 
Safety_test <- function(test = Test){
   res <- "Unsafe"
   if (all(test > 0) | all(test < 0)) {
      if (all(abs(test)<=3)){
         res <- "Safe"
      }
   }
   return(res)
}
Safety <- rep(NA, length(d))

for (i in 1:length(d)){
   Test <- diff(d[[i]])
   Safety[i] <- Safety_test(Test)
}
sum(Safety == "Safe")

Safety_damp <- rep(NA, length(d))
for (i in 1:length(d)){
   Test <- diff(d[[i]])
   res <- Safety_test(Test)
   if (res == "Safe") {
      Safety_damp[[i]] <- res
   } else {
      res <- rep(NA, length(d[[i]]))
      for(j in 1:length(d[[i]])){
         Test_damp <- diff(d[[i]][-j])
         res[j] <- Safety_test(Test_damp)
      }
      if(any(res == "Safe")){
      Safety_damp[[i]] <- "Safe"   
      } else{
         Safety_damp[[i]] <- "Unsafe"   
      }
   }
   }
sum(Safety_damp == "Safe")
