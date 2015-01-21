# Jinliang Yang
# Jan. 20th, 2015

library("nlme")
### an R function use nlme package to fit mixed model.
mixed_model <- function(data = df, model = KRN ~ Pedigree, random = ~1 | Farm/Rep, 
                        trait = "KRN") {
    
    trait <- as.character(model)[2]
    data <- data[!is.na(data[, trait]), ]
    data[, trait] <- as.numeric(as.character(data[, trait]))
    
    lmeout1 <- lme(model, data = data, random = random)
    ped.hat1 <- lmeout1$coef$fixed
    ped.hat1[-1] <- ped.hat1[-1] + ped.hat1[1]
    
    fix <- as.character(model)[3]
    names(ped.hat1)[1] <- data[order(data[, fix]), fix][1]
    names(ped.hat1) <- gsub(fix, "", names(ped.hat1))
    tped <- data.frame(Genotype = names(ped.hat1), trait = ped.hat1)
    names(tped)[2] <- trait
    return(tped)
}

### apply the mixed model
moon <- read.csv("data/final_all.csv")


fx <- as.formula(paste("t1", "~ Lunar.day", sep = " "))
rdm <- as.formula("~1|Genotype/Replicate")
gra <- subset(moon, !is.na(Lunar.day))
myres <- mixed_model(data = moon, model = fx, random = rdm, trait = "t1")

for (ti in names(gra)[6:65]) {
    fx <- as.formula(paste(ti, "~ Lunar.day", sep = " "))
    rdm <- as.formula("~1|Genotype/Replicate")
    tem <- mixed_model(data = gra, model = fx, random = rdm, trait = ti)
    myres <- merge(myres, tem, by = "Genotype")
}


par(mfrow = c(1, 1))
plot(x = 1, y = 1, type = "n", xlim = c(8, 37), ylim = c(-2, 50), xaxt = "n", 
     bty = "n", xlab = "Day after new moon", ylab = "Root Angle")
abline(v = c(8, 15, 22, 29, 37))
axis(side = 1, at = c(8, 15, 22, 29), tick = FALSE, cex.axis = 1, labels = c("1st Quarter", 
                                                                             "Full Moon", "Last Quarter", "New Moon"))

myres$Genotype <- as.numeric(as.character(gsub("L", "", myres$Genotype)))
# change to order to start with 1st quarter
a <- myres$Genotype
a[a < 8] <- a[a < 8] + 30
myres$Genotype <- a

for (i in 2:62) {
    smoothingSpline = smooth.spline(myres$Genotype, myres[, i], spar = 0.6)
    lines(smoothingSpline, lwd = 2, col = "red")
}