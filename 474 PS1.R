rm(list=ls()) #<- gets rid of all the things in your environment
ladata = read.csv("lalonde.csv", header = TRUE, na.strings=".")
attach(ladata)


# linear regression re78  = a + b treat + e
reg1 = lm(ladata$re78~ladata$treat, data=ladata)  #correct!!
summary(reg1)


summary(lm(re78~treat+ladata*treat*ladata)

#What did 'draft' represent in lecture 7? How would that translate inn problem set?
# Why did you write "veteran+draft+veteran*draft"?
#Use I in front if it is raised to a power more than 1!!!

#s.e under heteroscedesticity (robust)
reg1 = lm(re78~treat, data=ladata)
coeftest(reg1, vcov = sandwich)


# s.e under homoscedesticity (default)
summary(reg1)

names(ladata)
sd(re78)
#for lnre78 if needed
ladata$lnre78 = log(re78)

mean(re78) - mean(treat)

mean(re78[treat=="1"]) - mean(re78[treat=="0"])
confint(lm(re78~treat,level = 0.95))

