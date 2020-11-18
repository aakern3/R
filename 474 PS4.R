attend = read.table("attend.txt", header = F, na.strings=".")
attach(attend)
print(attend)

attend = V1
termGPA =V2
priGPA = V3
ACT = V4
final = V5
atndrte = V6
hwrte = V7
frosh = V8
soph = V9
skipped = V10
stndfnl = V11



summary(atndrte)
summary(priGPA)
summary(ACT)

prob4 = lm(atndrte~priGPA+ACT)
summary(prob4)

model=lm(atndrte~priGPA+ACT)
predval=predict(model, list(priGPA=3.65,ACT=20))
predval[2]

predvalu=predict(model, list(priGPA=3.1,ACT=21))
predvalu

predvalue =predict(model, list(priGPA=2.1,ACT=26))
predvalue

length(atndrte[priGPA=="3.65"&ACT=="20"])

detach(attend)

price = read.table("hprice1.txt", header = F, na.strings =".")
attach(price)
price = V1
assess = V2
bdrms = V3
lotsize = V4
sqrft = V5
colonial = V6
lprice = V7
lassess = V8
llotsize = V9
lsqrft = V10

model=lm(price~sqrft+bdrms)
summary(model)

detach(price)

rm(list=ls())

wage = read.table("wage2.txt", header = F, na.strings = ".")
attach(wage)

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

model1 = lm(IQ~educ)
summary(model1)
delta1til= coef(model1)[2]
delta1til

model2 = lm(lwage~educ)
beta1til = coef(model2)[2]
beta1til

model3 = lm(lwage~educ+IQ)
beta1hat = coef(model3)[2]
beta1hat
beta2hat = coef(model3)[3]
beta2hat

beta1til==beta1hat + beta2hat*delta1til