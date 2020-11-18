#econ 474 problem set 3
lalonde = read.csv("lalonde.csv", header = TRUE)

#create a dummy variable (==1 if re78>0)
lalonde$re78>0
lalonde$u78 = as.numeric(lalonde$re78>0)
#or
lalonde$u78 = ifelse(lalonde$re78>0, 1, 0)

#statistics
names(lalonde)
mean(lalonde$education)
sd(lalonde$education
summary(lalonde)
#simple way to calculate all means and sd of the variables in the data set at once
apply(lalonde, 2, mean)
apply(lalonde, 2, sd)
apply(lalonde, 1, mean)

#combine the mean and sd together in a table
rbind(apply(lalonde, 2, mean), apply(lalonde,2, sd))
#or use cbind^^^

#calculate the mean and sd by group
mean_gp = aggregate(lalonde, by = list(lalonde$treat), mean)
sd_gp = aggregate(lalonde, by = list(lalonde$treat), sd)

#create stat table
stat_tab = matrix(rep(NA, 66), 11, 6)
stat_tab
stat_tab[,1] = t(mean_gp[1,-c(1,2,11)])
stat_tab[,2] = t(sd_gp[1,-c(1,2,11)])
stat_tab[,3] = t(mean_gp[2,-c(1,2,11)])
stat_tab[,4] = t(sd_gp[2,-c(1,2,11)])
stat_tab[,5] = stat_tab[,3] - stat_tab[,1]
stat_tab[,6] = stat_tab[,5]/sqrt((stat_tab[,2]^2 + stat_tab[,4]^2)/2)

#regression with robust standard error

install.packages("lmtest"); require(lmtest)
install.packages("sandwich"); library(sandwich)

reg1 = lm(re78~treat, data = lalonde)
reg2 = lm(re78~treat+re74+re75+u74+u75, data = lalonde)
reg3 = lm(re78~treat+black+hispanic+age+married+nodegree+education+re74+re75+u74+u75, data = lalonde)

coeftest(reg1, vocv = sandwich)
coeftest(reg2, vcov = sandwich)
coeftest(reg3, vcov = sandwich)

options(max.print = 99999)

psid = read.csv("psid.csv", header = TRUE)
rm(list=ls())

#or
psid$u74 = ifelse(psid$re74>0, 1, 0)
psid$u75 = ifelse(psid$re75>0, 1, 0)
psid$u75


mean_gp = aggregate(psid, by = list(psid$treat), mean)
sd_gp = aggregate(psid, by = list(psid$treat), sd)

stat_tab = matrix(rep(NA,72), 12, 6)
stat_tab[,1] = t(mean_gp[1, -c(1,2,3)])
stat_tab[,2] = t(sd_gp[1, -c(1,2,3)])
stat_tab[,3] = t(mean_gp[2,-c(1,2,3)])
stat_tab[,4] = t(sd_gp[2,-c(1,2,3)])
stat_tab[,5] = stat_tab[,3] - stat_tab[,1]
stat_tab[,6] = stat_tab[,5]/sqrt((stat_tab[,2]^2 + stat_tab[,4]^2)/2)

install.packages("lmtest"); require(lmtest)
install.packages("sandwich"); library(sandwich)

reg1 = lm(re78~treat, data=psid)
reg2 = lm(re78~treat+re74+re75+u74+u75, data=psid)
reg3 = lm(re78~treat+re74+u74+re75+u75+black+hispanic+age+married+nodegree+education, data = psid)

coeftest(reg1, vcov = sandwich)
coeftest(reg2, vcov = sandwich)
coeftest(reg3, vcov = sandwich)

