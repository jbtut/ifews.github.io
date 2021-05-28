

dev.off()
par(mfrow=c(3,4), mar=rep(1,4)) ## plot all 9 submodels
x <- joined_countyXday[,-c(1,2)] 
dim(x)
for (i in 1:ncol(x)){
  hist(x[,i] %>% as.matrix(),
       main = colnames(x)[i])
}


joined_AgDistrictXday
joined_AgDistrictXmonth
joined_AgDistrictXyear
joined_countyXday
joined_countyXmonth
joined_countyXyear
joined_stateXday
joined_stateXmonth
joined_stateXyear