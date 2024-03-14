#import data from Excel file to RStudio#
install.packages("readxl")
library(readxl)
data1 <- read_excel("C:/Users/monih/OneDrive - NTHU/NTHU/3. Third Semester/Quantitative Research Methods in Marketing/Project/Shein Customers’ Perceptions (Responses)_Combined final.xlsx")

#EXPLORATORY FACTOR ANALYSIS
#load and use three packages for EFA#
install.packages("corpcor")
install.packages("GPArotation")
install.packages("psych")
library(corpcor)
library(GPArotation)
library(psych)

#before EFA: Bartlett's test#
cortest.bartlett(data1)
#as the p-value is 0, EFA is appropiate

#before EFA: check eigenvalue, which decides the number of factors in the command#
ev <- eigen(cor(data1))
ev$values
#as there are 6 eigen values greater than 0, it means we have 6 factors matching the model

#EFA_step 1: use all the questions for the variables in the model#
data2<-subset(data1, select=-c(ever_shopped_in_Shein, Gender, Age, Job_Status))
pc1<-principal(data2,nfactors=6,rotate="varimax")
print.psych(pc1,digits=3, sort=TRUE)
#Do not drop any questions even though BL and BP are under the same factor

#RELIABILITY TESTS
#Cronbach’s 𝜶:specify which question belongs to which variable #
fBL<-data2[,c('BL1','BL2','BL3','BL4','BL5','BL6')]
fBP<-data2[,c('BP1','BP2','BP3','BP4','BP5','BP6')]
fEA<-data2[,c('EA1','EA2','EA3','EA4','EA5','EA6','EA7')]
fEW<-data2[,c('EW1','EW2','EW3','EW4','EW5','EW6','EW7')]
fSE<-data2[,c('SE1','SE2','SE3','SE4','SE5','SE6')]
fCon<-data2[,c('Con1','Con2','Con3','Con4','Con5','Con6','Con7')]

#Examine each variable's Cronbach’s #
alpha(fBL)
alpha(fBP)
alpha(fEA)
alpha(fSE)
alpha(fCon)

#alpha BL 0.94 , BP 0.94, EA 0.96, SE 0.95, Con 0.90 

#VALIDITY TESTS
#create variables
attach(data2)
data2$loyalty <- (BL1+BL2+BL3+BL4+BL5+BL6)/6
data2$preference <- (BP1+BP2+BP3+BP4+BP5+BP6)/6
data2$environment <- (EA1+EA2+EA3+EA4+EA5+EA6+EA7)/7
data2$employee <- (EW1+EW2+EW3+EW4+EW5+EW6+EW7)/7
data2$social <- (SE1+SE2+SE3+SE4+SE5+SE6)/6
data2$cons <- (Con1+Con2+Con3+Con4+Con5+Con6+Con7)/7
detach(data2)

#examine correlation coefficients of variables
library(corrplot)
a = cor(data2[,c(40:45)], method = "pearson", use = "pairwise.complete.obs")
b = cor(data2[,c(40:45)], method = "spearman", use = "pairwise.complete.obs")
corrplot(a, method="number", addCoef.col="black", number.digits = 4,number.cex=0.8, tl.cex = 0.8)
corrplot(b, method="number", addCoef.col="black", number.digits = 4,number.cex=0.8, tl.cex = 0.8)

#implement confirmatory factor analysis (CFA)
#install the package for CFA, lavaan
install.packages("lavaan", dependencies = TRUE)
library(lavaan)

#create factors/variables and their questions
m1 <- 'f1 =~ BL1+BL2+BL3+BL4+BL5+BL6
       f2 =~ EA1+EA2+EA3+EA4+EA5+EA6+EA7
       f3 =~ EW1+EW2+EW3+EW4+EW5+EW6+EW7
       f4 =~ SE1+SE2+SE3+SE4+SE5+SE6
       f5 =~ BP1+BP2+BP3+BP4+BP5+BP6
       f6 =~ Con1+Con2+Con3+Con4+Con5+Con6+Con7'

#implement CFA#
cfa1 <- cfa(m1, data=data2) 
summary(cfa1, fit.measures=TRUE, standardized=TRUE)

#CFI 0.974 greater than 0.9 --- TLI 0.972 greater than 0.9 
#RMSEA 0.038 smaller than 0.08 --- SRMR 0.041 smaller than 0.08

#MAIN EFFECT ANALYSIS
#descriptive statistics#
data3 <- data1

#create variables
attach(data3)
data3$BL <- (BL1+BL2+BL3+BL4+BL5+BL6)/6
data3$BP <- (BP1+BP2+BP3+BP4+BP5+BP6)/6
data3$EA <- (EA1+EA2+EA3+EA4+EA5+EA6+EA7)/7
data3$EW <- (EW1+EW2+EW3+EW4+EW5+EW6+EW7)/7
data3$SE <- (SE1+SE2+SE3+SE4+SE5+SE6)/6
data3$Con <- (Con1+Con2+Con3+Con4+Con5+Con6+Con7)/7
detach(data3)

summary(data3)
summary(data3$BL)
summary(data3$EA)
summary(data3$EW)
summary(data3$SE)
summary(data3$BP)
summary(data3$Con)

#descriptive statistics: use the "vtable" package#
#source:https://cran.r-project.org/web/packages/vtable/vignettes/sumtable.html#
install.packages("vtable")
library(vtable)
st(data3)
st(data3,digits=2)
st(data3, vars = c('BL','EA','EW','SE','BP','Con'),digits=2)
st(data3, vars = c('BL','EA','EW','SE','BP','Con'),digits=2,summ=c('notNA(x)','mean(x)','sd(x)'))

#get the frequencies of gender, age, and job status#
install.packages("plyr")
library(plyr)
count(data3$Gender)
count(data3$Age)
count(data3$Job_Status)

#multivariate linear regression: BL, EA, EW and SE. Do not include any control variables.#
lmBL1=lm(BL~EA+EW+SE, data=data3)
summary(lmBL1)

#multivariate linear regression: BL, EA, and SE. With control variables gender, age and job.#
lmBL2=lm(BL~EA+EW+SE+Gender+Age+Job_Status, data=data3)
summary(lmBL2)

#MODERATOR ANALYSIS
#mean-center EA, EW, SE, and Con#
attach(data3)
data3$c_EA <- EA-3
data3$c_EW <- EW-3.1
data3$c_SE <- SE-3.2
data3$c_Con <- Con-3.8
detach(data3)

#check if the means of EA, EW, SE, and Con are close to zero#
st(data3, vars = c('EA','c_EA','EW','c_EW','SE','c_SE','Con','c_Con'),digits=2,summ=c('notNA(x)','mean(x)'))

#create 3 new variables to check the moderation effects, using mean-centered variables#
attach(data3)
data3$Envicon <- c_EA*c_Con
data3$Empcon <- c_EW*c_Con
data3$Socicon <- c_SE*c_Con
detach(data3)

#analyse the moderation model, using the mean-centered
lmBL3=lm(BL~c_EA+c_EW+c_SE+c_Con+Gender+Age+Job_Status+Envicon+Empcon+Socicon, data=data3)
summary(lmBL3)

#check if the moderation model above has the multicollinearity concern, using the mean-centered
install.packages("faraway")
library(faraway)
vif(lmBL3)

#vif values are less than 10, there is no multicollinearity

#MEDIATOR ANALYSIS
###Baron & Kenny###
#install the lavaan package#
install.packages("lavaan", dependencies = TRUE)
library(lavaan)
#step 1: get c'#
m1 <- 'BL ~ EA+EW+SE+Gender+Age+Job_Status'
sem1 <- sem(m1, data=data3) 
summary(sem1, standardized=TRUE)

#step 2: get a1, a2, a3 (x->m), b (m->y), c1, c2, c3(x->y,direct effect of x on y), and a*b (indirect effect of x on y)#
m2 <- 'BP ~ a1*EA+a2*EW+a3*SE+Gender+Age+Job_Status
       BL ~ c1*EA+c2*EW+c3*SE+b*BP+Gender+Age+Job_Status
       a1b := a1*b
       a2b := a2*b
       a3b := a3*b'
sem2 <- sem(m2, data=data3) 
summary(sem2, standardized=TRUE)

###ZLC(2010)###
#step 1: get a(x->m), b(m-<y), c(x->y,direct effect of x on y), a*b (indirect effect of x on y, and a*b*c#
m3 <- 'BP ~ a1*EA+a2*EW+a3*SE+Gender+Age+Job_Status
       BL ~ c1*EA+c2*EW+c3*SE+b*BP+Gender+Age+Job_Status
       a1b := a1*b
       a2b := a2*b
       a3b := a3*b
       a1bc1 := a1*b*c1
       a2bc2 := a2*b*c2
       a3bc3 := a1*b*c3'
sem3 <- sem(m3, data=data3) 
summary(sem3, standardized=TRUE)