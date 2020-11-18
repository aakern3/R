meap01 = read.table("meap01.txt", header=F, na.string=".")
attach(meap01)
math4=V3
dcode=V1
bcode=V2
read4=V4
lunch=V5
enroll=V6
expend=V7
exppp=V8

summary(math4)
length(math4[math4=="100"])
length(math4[math4=="50"])
mean(math4)
mean(read4)
cor(math4,read4)
mean(exppp)
sd(exppp)
detach(meap01)

jtrain2 = read.table("jtrain2.txt", header=F, na.string=".")
attach(jtrain2)
train=V1
age=V2
educ=V3
black=V4
hisp=V5
married=V6
nodegree=V7
mosinex=V8
re74=V9
re75=V10
re78=V11
unem74=V12
unem75=V13
unem78=V14
lre74=V15
lre75=V16
lre78=V17
agesq=V18
mostrn=V19

length(train[train=="1"])
mean(re78[train=="1"])
mean(re78[train=="0"])

length(unem78[unem78=="1"&train=="1"])
length(unem78[unem78=="1"&train=="0"])
detach(jtrain2)

ceosal2 = read.table("ceosal2.txt", header = F, na.strings=".")
attach(ceosal2)
salary=V1
age=V2
college=V3
grad=V4
comten=V5
ceoten=V6
sales=V7
profits=V8
mktval=V9
lsalary=V10
lsales=V11
lmktval=V12
comtensq=V13
ceotensq=V14
profmarg=V15

mean(salary)
mean(ceoten)
length(ceoten[ceoten=="0"])
max(ceoten)
lm(lsalary~ceoten)
detach(ceosal2)

wage2 = read.table("wage2.txt", header = F, na.strings=".")
attach(wage2)
wage=V1
hours=V2
IQ=V3
KWW=V4
educ=V5
exper=V6
tenure=V7
age=V8
married=V9
black=V10
south=V11
urban=V12
sibs=V13
brthord=V14
meduc=V15
feduc=V16
lwage=V17

mean(wage)
mean(IQ)
lm(wage~IQ)
summary(lm(wage~IQ))
sd(IQ)
lm(lwage~IQ)
summary(lm(lwage~IQ))
detach(wage2)

meap93 = read.table("meap93.txt", header=F, na.strings=".")
attach(meap93)

lnchprg=V1
enroll=V2
staff=V3
expend=V4
salary=V5
benefits=V6
droprate=V7
gradrate=V8
math10=V9
sci11=V10
totcomp=V11
ltotcomp=V12
lexpend=V13
lenroll=V14
lstaff=V15
bensal=V16
lsalary=V17

lm(math10~expend)
summary(lm(math10~expend))
lm(math10~lexpend)
summary(lm(math10~lexpend))
max(expend)
detach(meap93)

x = runif(500,0,10)
u = rnorm(500,0,6)
mean(x)
sd(x)
mean(ui)
u1=rnorm(500,0,6)
mean(u)
sd(u)
y = 1+2*x+u
a=lm(y~x)
summary(a)
uhat=resid(a)
sum(uhat)
sum(x*uhat)
sum(u)
sum(x*u)
y=1+2*x+u
lm(y~x)
summary(lm(y~x))


draft_data = read.csv("draft.csv", header = TRUE)
reg1 = lm(lnwage~veteran, draft = draft_data)
summary(reg1)
summary(lm(lnwage~veteran+draft*veteran*draft, data=draft_data))
# line under this is supposed to work and replace row above it
summary(lm(lnwage~veteran*draft))

summary(lm(lnwage~veteran+draft*veteran*draft + birthyr + I(birthyr^2), data=draft_data))
#need to innclude the I for polynomial terms

#standard error under heteroscedasticity:
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
require(sandwich)

reg1 <- lm(lnwage~veteran, data=draft_data)
# heteroscedasticity s.e (robust)
coeftest(reg1, vcov = sandwich)
# homoscedasticity s.e (default)
summary(reg1)

out1 <- summary(reg1)

sqrt(out1$sigma^2/ (var(draft_data$veteran)*(dim(draft_data)[1]-1)))


v = (draft_data$veteran - mean(draft_data$veteran)*reg1$residuals)
#this isnt finished (line undernneath this)
sqrt(sum(v^2)/(var(draft_data$veteran)*(dim(draft_data)[1]-1)))