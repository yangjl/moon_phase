#####
# moonphase_mlm.R
# Author: Aaron Kusmec
# Date: 01.07.2015
# Revised: 01.08.2015
#
# MLM analysis of root angle/moon phase data from Spalding lab.

library(lme4)

source("lib/rsquaredglmm.R")
moon <- read.csv("data/final_all.csv")

# Structures to hold results
r2marg <- data.frame(trait=vector(), r2marg=vector(), r2cond=vector())
sig <- data.frame(trait=vector(), chisq=vector(), df=vector(), p.value=vector())

# Construct MLMs for each of the 61 time points
for (i in 5:65) {
  trait <- names(moon)[i]
  
  # Construct the null model
  null.formula <- as.formula(paste(trait, "~ (1 | Genotype/Replicate)", sep=" "))
  model.null <- lmer(null.formula, data = moon, REML = FALSE)
  
  # Construct the model
  full.formula <- as.formula(paste(trait, "~ Lunar.day + (1 | Genotype/Replicate)", sep=" "))
  model.full <- lmer(full.formula, data = moon, REML = FALSE)
  
  # Retrieve the p-value
  inter1a <- anova(model.null, model.full)[2, 6:8]
  inter1b <- data.frame(trait=trait, chisq=inter1a[1, 1], df=inter1a[1, 2], p.value=inter1a[1, 3])
  sig <- rbind(sig, inter1b)
  
  # Calculate the marginal R-squared
  inter2a <- rsquared.glmm(model.full)
  inter2b <- data.frame(trait=trait, r2marg=inter2a[1, 4], r2cond=inter2a[1, 5])
  r2marg <- rbind(r2marg, inter2b)
}

# Calculate q-values
sig$q.value <- p.adjust(sig[, "p.value"], method="fdr")

time.points <- seq(0, 180, 3)

# Plot R-squared vs. time point
plot(time.points, r2marg$r2marg, main="Variance Explained by Lunar Phase", 
     xlab="Time after Gravity Stimulation (min)", ylab="R^2", pch=20)

# Plot q-value vs. time point
# Use 0.05 cut-off
plot(time.points, -log(sig$q.value, 10), main="Q-values of Lunar Phase",
     xlab="Time after Gravity Stimulation (min)", ylab="-log10(q-value)", pch=20)
abline(h=-log(0.05, 10), lty=2, col="red", lwd=2)

# Save the analysis results
write.csv(cbind(sig, r2marg$r2marg, r2marg$r2cond), "moonphase_results.csv", row.names=FALSE, quote=FALSE)
