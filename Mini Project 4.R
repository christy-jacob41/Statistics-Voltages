library(tidyverse)
library(boot)

# reading in gpa data
gpa.data <- read_csv("gpa.csv")

# getting gpa and act values
gpas <- gpa.data$gpa
acts <- gpa.data$act

# making a scatterplot
plot(gpas, acts, main = "GPAs and ACT Scores", xlab = "GPA", ylab = "ACT Scores")

# making best fit line
abline(lm(acts~gpas))

# finding correlation
cor(gpas, acts)

# custom function to calculate correlation
cor.npar <- function(gpa, indices)
{
  gpa.val <- gpa$gpa[indices]
  act.val <- gpa$act[indices]
  result <- cor(gpa.val, act.val)
  return(result)
}

# using bootstrap to get point estimate, bias, and SE
cor.npar.boot <- boot(gpa.data, cor.npar, R=999, sim="ordinary", stype="i")
cor.npar.boot

# getting the point estimate
mean(cor.npar.boot$t)

# 95% confidence interval using percentile bootstrap
boot.ci(cor.npar.boot)

# getting first and third quantiles from bootstrap
sort(cor.npar.boot$t)[c(25,975)]

# question 2
# read data from voltage csv
voltage <- read_csv("VOLTAGE.csv")

# getting data for remote and local voltage
remote.voltage <- voltage$voltage[which(voltage$location==0)]
local.voltage <- voltage$voltage[which(voltage$location==1)]

# drawing a side by side boxplot of the two distributions
boxplot(remote.voltage, local.voltage, names = c("Remote Voltage", "Local Voltage"))
summary(remote.voltage)
summary(local.voltage)

# drawing qqplots for the distribution
qqnorm(remote.voltage, main="Remote Voltage")
qqline(remote.voltage)
qqnorm(local.voltage, main="Local Voltage")
qqline(local.voltage)

# getting the means
rm <- mean(remote.voltage)
lm <- mean(local.voltage)

# getting the variances
rv <- var(remote.voltage)
lv <- var(local.voltage)

# getting the differences of the means
dif <- rm-lm
dif

# getting the standard error
se <- sqrt(rv/30 + lv/30)
se

# getting the confidence interval
ci <- dif + c(-1,1) * qnorm(.975) * se
ci

# conducint a t test 
t.test(remote.voltage, local.voltage, alternative = "two.sided", paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# question 3
# reading data from vapor.csv
vapor <- read_csv("VAPOR.csv")

# getting theoretical and experimental data
the <- vapor$theoretical
exp <- vapor$experimental

# getting qqplots
qqnorm(the, main = "Theoretical")
qqline(the)
qqnorm(exp, main = "Experimental")
qqline(exp)

# getting boxplot
boxplot(the, exp, names=c("Theoretical", "Experimental"))

# getting summary statistics
summary(the)
summary(exp)

# getting the mean of the difference
mean(the)-mean(exp)

# getting the sd of the difference
vap.dif <- the-exp
sd(vap.dif)

# getting the confidence interval
(mean(the)-mean(exp)) + c(-1,1) * qt(0.975, 15) * sd(vap.dif)/sqrt(16)

# t test
t.test(the, exp, alternative = "two.sided", paired=TRUE, var.equal = FALSE, conf.level = 0.95)
