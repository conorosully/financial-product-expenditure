#Actuarial Science Research Project
#Conor O'Sullivan
#26 July 2017

setwd("~/Desktop/Project/csv")
source("~/Desktop/Project/fun.R")
library(plyr)
library(ggplot2)
library(statmod)
library(MASS)
library(heatmapFit)
library(car)
#dev.off()


#OBTAINING DATA
#==================================
options(digits=20)
dataTotal <- read.csv(file="ies_2010_2011_total_v1.csv", header=TRUE, sep=",", numerals = "no.loss")
dataHInfo <- read.csv(file="ies_2010_2011_house_info_v1.csv", header=TRUE, sep=",", numerals = "no.loss")
dataPInfo <- read.csv(file="ies_2010_2011_person_info_v1.csv", header=TRUE, sep=",", numerals = "no.loss")
dataIInfo <- read.csv(file="ies_2010_2011_person_income_v1.csv", header=TRUE, sep=",", numerals = "no.loss")

colnames(dataTotal)
colnames(dataHInfo)
colnames(dataPInfo)
colnames(dataIInfo)

#The number of rows should all be equal to the "cases" in the IES documentation
nrow(dataTotal) #1298446
nrow(dataPInfo) #95042
nrow(dataIInfo) #55800
nrow(dataHInfo) #25328

#The number of unique IDs should be equal to the total number of households
length(unique(dataTotal$UQNO))
length(unique(dataHInfo$UQNO))
#==================================


#COICOP CODES
#==================================
insurance = c(12521010,12521020,12521030,12531020,12531110,12531200,
              12541100, 12551000,12551020)
services = c(12621010,12621020,12621040)
UIF = c(53300000)
savings = c(52320000, 52410000,52421000,52500000)
debt = c(70100000,70101000, 70110000, 70111000,70120000,70121000, 70200000,70210000, 
         70300000,70310000,70400000,70410000,70500000,70510000,70600000,70610000,70700000,
         70710000,70800000, 70810000,70900000,70910000)
all = c(insurance,services,UIF,savings,debt)

funPol = c(12551020)
stokvel = c(52500000)
pension = c(52421000)
medical = c(12531020, 12531110)
#==================================

#Data Exploration
#==================================
d_all <-df(all)

check(d_all,all)
head(d_all)
names(d_all)
d_all

d_ins <-df(insurance)
head(d_ins)

d_services <- df(services)
d_UIF <- df(UIF)
d_savings <- df(savings)
d_debt <- df(debt)

d_funPol <- df(funPol)
d_stokvel <- df(stokvel)
d_pension <- df(pension)

d_med <- df(medical)


#Assessing distribution of response
dataframe = d_med #change when dealing with a new data set
dataframe. = dataframe[dataframe$bin==1,] 
y = dataframe$expenditure
y. = dataframe.$expenditure

paste("Number of zeros: ", sum(y==0))
#3456 for all
#9120 for insurance
#9117 for service
#19176 for UIF
#17809 for savings
#13504 for debt
#11260 for funPol
#22833 for stokvel
#20432 for pension
#24641 for interest 
#20870 for medical
zeros = c(3456,9120,9117,19176,17809,13504)
25328 -zeros 
(25328-zeros)*100/25328

p <- ggplot(dataframe, aes(row, expenditure, colour = as.factor(bin))) + geom_point()
print(p)


#potential interactions
names(dataframe)
attach(dataframe)
par(mfrow = c(1,1))
psymb<-as.numeric(bin)
#workIncome and otherIncome 
plot(workIncome~otherIncome, pch= psymb)
legend("topright",legend=c("0","1"), pch=c(0,1), title = "clm")
cor(workIncome,otherIncome)
#inKindIncome and employerCont
plot(inKindIncome~employerCont, pch= psymb)
legend("topright",legend=c("0","1"), pch=c(0,1), title = "clm")
cor(inKindIncome,employerCont)
#popGrpOfHead and hSize
boxplot(hSize~as.factor(popGrpOfHead)*bin,ylab = "hSize",xlab = "popGrpOfHead*bin", main = "hSize and popGrpOfHead")
#personsSupported and hSize
plot(personsSupported~hSize, pch= psymb)
boxplot(hSize~as.factor(personsSupported)*bin,ylab = "hSize",xlab = "popGrpOfHead*bin", main = "hSize and popGrpOfHead")
legend("topright",legend=c("0","1"), pch=c(0,1), title = "clm")
cor(personsSupported,hSize)
#settlementType and popGrpOfHead
table1 = xtabs(bin~as.factor(settlementType)+as.factor(popGrpOfHead))
table1
prop.table(table1,1)*100 #row % 
prop.table(table1, 2)*100 #col %
#province and popGrpOfHead
table2 = xtabs(bin~as.factor(province)+as.factor(popGrpOfHead))
table2
prop.table(table2,1)*100 #row % 
prop.table(table2, 2)*100 #col %

#incomeDecile and popGrpOfHead
table3 = xtabs(bin~as.factor(incomeDecile)+as.factor(popGrpOfHead))
table3
prop.table(table3,1)*100 #row % 
prop.table(table3, 2)*100 #col %
#incomeDecile and genderOfHead
table4 = xtabs(bin~as.factor(incomeDecile)+as.factor(genderOfHead))
table4
prop.table(table3,1)*100 #row % 
prop.table(table3, 2)*100 #col %
#incomeDecile and province
table5 = xtabs(bin~as.factor(incomeDecile)+as.factor(province))
table5
prop.table(table5,1)*100 #row % 
prop.table(table5, 2)*100 #col %

#INTERACTIONS TO BE FITTED:
#settlementType and popGrpOfHead,
#province and popGrpOfHead 
#incomeDecile and popGrpOfHead
#incomeDecile and genderOfHead

#==================================
#MODEL FITTING
#==================================
#coicop: all
options(digits=2)
lmA = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
             employerCont+as.factor(province) +as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
           data=d_all,  family= binomial(link = "logit"))

lmA1 <- step(lmA, direction="both")
summary(lmA1)
heatmap.fit(y=d_all$bin,pred=fitted(lmA1))#no significant deviations
vif(lmA1)#no significant multicollinearity

lmA. = glm(bin ~income+inKindIncome+
            employerCont+as.factor(province) +as.factor(settlementType)+hSize+
            as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
          data=d_all,  family= binomial(link = "logit"))

lmA2 <- step(lmA., direction="both")
summary(lmA2)
heatmap.fit(y=d_all$bin,pred=fitted(lmA2))#can NOT accept
vif(lmA2)#province highest VIF


lmA3 = glm(bin ~income+I(income^2) +inKindIncome+
             employerCont+as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
           data=d_all,  family= binomial(link = "logit"),control = list(maxit = 1000))
#Problems with algorithm convergence - may be due to perfect separation
summary(lmA3)
heatmap.fit(y=d_all$bin,pred=fitted(lmA3))#can NOT accept
vif(lmA3)

AIC(lmA1,lmA2,lmA3)
#use lmA1

#coicop: insurance
lmI = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
            employerCont+as.factor(province) +as.factor(settlementType)+hSize+
            as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
          data=d_ins,  family= binomial(link = "logit"))
lmI1 <- step(lmI, direction="both")
summary(lmI1)
heatmap.fit(y=d_ins$bin,pred=fitted(lmI1))#Can NOT accept the specification
vif(lmI1)#province has highest VIF 



#remove province
lmI2 = glm(bin ~ as.factor(incomeDecile)+
            employerCont +as.factor(settlementType)+hSize+
            as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
          data=d_ins,  family= binomial(link = "logit"))

heatmap.fit(y=d_ins$bin,pred=fitted(lmI2))#Can NOT accept the specification
vif(lmI2)

#add inKindIncome
lmI3 = glm(bin ~ as.factor(incomeDecile)+ inKindIncome+
             employerCont +as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
           data=d_ins,  family= binomial(link = "logit"))
heatmap.fit(y=d_ins$bin,pred=fitted(lmI3))#Can accept the specification
vif(lmI3)#No significant multicollinearity


lmI. = lmI = glm(bin ~ income +inKindIncome+
                   employerCont+as.factor(province) +as.factor(settlementType)+hSize+
                   as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
                 data=d_ins,  family= binomial(link = "logit"))
lmI4 <- step(lmI., direction="both")
summary(lmI4)
heatmap.fit(y=d_ins$bin,pred=fitted(lmI4))#Can NOT accept the specification
vif(lmI4)

lmI.. = lmI = glm(bin ~ income + I(income^2) +inKindIncome+
                   employerCont+as.factor(province) +as.factor(settlementType)+hSize+
                   as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
                 data=d_ins,  family= binomial(link = "logit"))
#problems with algorithm covergence


AIC(lmI1,lmI2,lmI3,lmI4)
#USE lmI3

#coicop: services
lmS = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
            employerCont+as.factor(province) +as.factor(settlementType)+hSize+
            as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
          data=d_services,  family= binomial(link = "logit"))
lmS1 <- step(lmS, direction="both")
summary(lmS1)
heatmap.fit(y=d_services$bin,pred=fitted(lmS1))#Can accept the specification
vif(lmS1)#no significant multicollinearity

lmS. = glm(bin ~ income+I(income^2)+inKindIncome+
            employerCont+as.factor(province) +as.factor(settlementType)+hSize+
            as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
          data=d_services,  family= binomial(link = "logit"))
lmS2 <- step(lmS., direction="both")
summary(lmS2)
heatmap.fit(y=d_services$bin,pred=fitted(lmS2))#Can NOT accept the specification
vif(lmS2)

AIC(lmS1,lmS2)
#USE lmS1

#coicop: funPol
lmFP = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
             employerCont+as.factor(province) +as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
           data=d_funPol,  family= binomial(link = "logit"))

lmFP1 <- step(lmFP, direction="both")
summary(lmFP1)
heatmap.fit(y=d_funPol$bin,pred=fitted(lmFP1))#Can accept the specification
vif(lmFP1)#no significant multicollinearity

lmFP. = glm(bin ~ income+I(income^2)+inKindIncome+
             employerCont+as.factor(province) +as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
           data=d_funPol,  family= binomial(link = "logit"))

lmFP2 <- step(lmFP., direction="both")
summary(lmFP2)
heatmap.fit(y=d_funPol$bin,pred=fitted(lmFP2))#Can NOT accept the specification
vif(lmFP2)#no significant multicollinearity

AIC(lmFP1,lmFP2)
#USE lmFP1


#coicop: stokvel
#may be difficult to produce a good model as less than 10% of responses have a 1 value
lmSV = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
             employerCont+as.factor(province) +as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
           data=d_stokvel,  family= binomial(link = "logit"))

lmSV1 <- step(lmSV, direction="both")
summary(lmSV1)
heatmap.fit(y=d_stokvel$bin,pred=fitted(lmSV1))#Can accept the specification
vif(lmSV1)#province has highest VIF

lmSV. = glm(bin ~ income+inKindIncome+
             employerCont+as.factor(province) +as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
           data=d_stokvel,  family= binomial(link = "logit"))
heatmap.fit(y=d_stokvel$bin,pred=fitted(lmSV.))
summary(lmSV.)

lmSV.. = glm(bin ~ income+I(income^2)+inKindIncome+
              employerCont+as.factor(province) +as.factor(settlementType)+hSize+
              as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
            data=d_stokvel,  family= binomial(link = "logit"))
heatmap.fit(y=d_stokvel$bin,pred=fitted(lmSV..))
summary(lmSV..)


lmSV = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
             employerCont+as.factor(province) +as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead)+ 
             as.factor(settlementType)*as.factor(popGrpOfHead)+
             as.factor(settlementType)*as.factor(province),
           data=d_stokvel,  family= binomial(link = "logit"))
lmSV2 <- step(lmSV, direction="both")
summary(lmSV2)
heatmap.fit(y=d_stokvel$bin,pred=fitted(lmSV2))#Can accept the specification
vif(lmSV2)


lmSV = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
             employerCont+as.factor(province) +as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead)+
             as.factor(incomeDecile)*as.factor(popGrpOfHead)+
             as.factor(incomeDecile)*as.factor(genderOfHead),
           data=d_stokvel,  family= binomial(link = "logit"))
lmSV3 <- step(lmSV, direction="both")
summary(lmSV3)
heatmap.fit(y=d_stokvel$bin,pred=fitted(lmSV3))#Can accept the specification
vif(lmSV3)#significant multicorrelations


AIC(lmSV1,lmSV2,lmSV3,lmSV.,lmSV..)

#Use lmSV1, lmSV3 has better heatmap but high multicorrelations

#coicop: pension
lmP = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
             employerCont+as.factor(province) +as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
           data=d_pension,  family= binomial(link = "logit"))

lmP1 <- step(lmP, direction="both")
summary(lmP1)
heatmap.fit(y=d_pension$bin,pred=fitted(lmP1))#Can NOT accept the specification
vif(lmP1)

#remove genderOfHead
lmP2 = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
            employerCont+as.factor(province) +as.factor(settlementType)+hSize+
            personsSupported+as.factor(genderOfHead),
          data=d_pension,  family= binomial(link = "logit"))
summary(lmP2)
heatmap.fit(y=d_pension$bin,pred=fitted(lmP2))#Can NOT accept the specification
vif(lmP2)

#use continuous income
lmP. = glm(bin ~ income+ I(income^2)+inKindIncome+
            employerCont+as.factor(province) +as.factor(settlementType)+hSize+
            as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
          data=d_pension,  family= binomial(link = "logit"))

lmP3 <- step(lmP., direction="both")
summary(lmP3)
heatmap.fit(y=d_pension$bin,pred=fitted(lmP3))#Can NOT accept the specification
vif(lmP3)

#add some interactions
lmP.. = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
             employerCont+as.factor(province) +as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead)+
            as.factor(settlementType)*as.factor(popGrpOfHead)+
              as.factor(settlementType)*as.factor(province),
           data=d_pension,  family= binomial(link = "logit"))
lmP4 <- step(lmP.., direction="both")
summary(lmP4)
heatmap.fit(y=d_pension$bin,pred=fitted(lmP4))#Can NOT accept the specification
vif(lmP4)

#try income interactions
lmP... = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
              employerCont+as.factor(province) +as.factor(settlementType)+hSize+
              as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead)+
               as.factor(incomeDecile)*as.factor(popGrpOfHead)+
               as.factor(incomeDecile)*as.factor(genderOfHead),
            data=d_pension,  family= binomial(link = "logit"))
lmP5 <- step(lmP..., direction="both")
summary(lmP5)
heatmap.fit(y=d_pension$bin,pred=fitted(lmP5))#Can NOT accept the specification
vif(lmP5)


AIC(lmP1,lmP2,lmP3,lmP4,lmP5)
#no suitable model could be fitted

#coicop: medical
lmM = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
             employerCont+as.factor(province) +as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
           data=d_med,  family= binomial(link = "logit"))
lmM1 <- step(lmM, direction="both")
summary(lmM1)
heatmap.fit(y=d_med$bin,pred=fitted(lmM1))#Can NOT accept the specification
vif(lmM1)

#add inKindIncome
lmM2 = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
            employerCont+as.factor(province) +as.factor(settlementType)+hSize+
            as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
          data=d_med,  family= binomial(link = "logit"))
summary(lmM2)
heatmap.fit(y=d_med$bin,pred=fitted(lmM2))#Can NOT accept the specification
vif(lmM2)

#remove province
lmM3 = glm(bin ~ as.factor(incomeDecile)+inKindIncome+
             employerCont +as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead),
           data=d_med,  family= binomial(link = "logit"))
summary(lmM3)
heatmap.fit(y=d_med$bin,pred=fitted(lmM3))#Can NOT accept the specification
vif(lmM3)

#try continuous income
lmM4 = glm(bin ~ as.factor(incomeDecile)+inKindIncome+as.factor(province)+
             employerCont +as.factor(settlementType)+hSize+
             as.factor(popGrpOfHead)+personsSupported+as.factor(genderOfHead)+
             as.factor(incomeDecile)*as.factor(genderOfHead)+
             as.factor(incomeDecile)*as.factor(popGrpOfHead)+
             inKindIncome*as.factor(popGrpOfHead),
           data=d_med,  family= binomial(link = "logit"))
#lmM4 <- step(lmM., direction="both")
summary(lmM4)
heatmap.fit(y=d_med$bin,pred=fitted(lmM4))#Can NOT accept the specification
vif(lmM4)

AIC(lmM1,lmM2,lmM3,lmM4)
#no suitable model could be fitted

