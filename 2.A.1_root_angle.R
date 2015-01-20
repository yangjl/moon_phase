# Jinliang Yang
# 8/25/2014
# root angle pheno re-analysis

ob <- load("~/Documents/schnable_lab/moon_phase/gravity.Rdata")

getpheno <- function(gra=gra){
  source("~/Documents/schnable_lab/moon_phase/mixed_model.R")
  message("calculate the BLUE of the trait...")
  ### apply the mixed model
  fx <- as.formula(paste("V1", "~ Genotype", sep = " "))
  rdm <- as.formula("~1|Rep/Batch")
  myres <- mixed_model(data = gra, model = fx, random = rdm, trait = "V1")
  
  for (ti in names(gra)[7:66]) {
    fx <- as.formula(paste(ti, "~ Genotype", sep = " "))
    rdm <- as.formula("~1|Rep/Batch")
    tem <- mixed_model(data = gra, model = fx, random = rdm, trait = ti)
    myres <- merge(myres, tem, by = "Genotype")
  }
  return(myres)
}

bluegra <- getpheno(gra=gra)

# source("~/Documents/Rcodes/save.append.R")
# save.append(list="bluegra", file="~/Documents/PhenoCamp/PCamp_proj/cache/gra_blue.Rdata",
#             description="gra after fitting MLM random: seeds and camera")
# #########################################
idx <- grep("x", bluegra$Genotype)
hybrid <- bluegra[idx, ] #211
inbred <- bluegra[-idx,] #24

tbluegra <- as.data.frame(t(bluegra))[-1,]
plot(x=3*(0:60), tbluegra[,1], type="l", lwd=3, col="grey", ylim=c(0, 70),
     main="Root tip angle", xlab="Time (mins)", ylab="Root tip angle (degree)")
for(i in 2:ncol(tbluegra)){
  lines(x=3*(0:60), tbluegra[,i], lwd=3, col="grey")
}
abline(h=0, col="black", lty=1, lwd=3)
abline(h=90, col="black", lty=2, lwd=3)


