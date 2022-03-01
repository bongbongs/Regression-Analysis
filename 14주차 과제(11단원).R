#연습문제 11.1
#(1)
n=25;p=3;MSE=0.78;MSR=1803.30
F0=MSR/MSE
F0
#F(1,n-p-1,0.05) = F(1,21,0.05) = 9.59
#F0=1803.30>9.59이므로 유의수준 0.05에서 타당하다.

#(2)
SSR=5409.89;SSE=16.35;SST=SSR+SSE

R2=1-(SSE/SST)
R2

R2a=1-((n-1)/(n-p-1))*(1-R2)
R2a

MSE=SSE/(n-p-1)
MSE

#Cp=(1/불편추정량)*SSE-n+2(p+1)

#R2값은 증가가 둔화되는 지점, R2a값은 최대, MSE,Cp값은 최소가 되는 변수(p)를 선택하는 것이 적합하다.


#연습문제 11.5
yi <- c(79,200,163,200,178,146,31,292,160,339,160,86,237,107,155)
x1i <- c(10,8,12,7,12,8,12,5,8,5,11,12,6,10,10)
x2i <- c(8,6,9,16,17,15,8,10,4,16,7,12,6,4,4)
x3i <- c(5.5,2.5,8.0,3.0,2.9,3.0,8.0,9.0,4.0,6.5,5.5,5.0,6.0,5.0,3.5)

#(1)
fit <- lm(yi~x1i+x2i+x3i)
anova(fit)
#변수 x1i에서만 유의하다.

#(2)
#install.packages('leaps')
library(leaps)
library(MASS)
fw <- stepAIC(fit,direction='forward')

summary(fw)

#y=330.94 - 23.993x1i

#(3)
bw <- stepAIC(fit,direction='backward')

summary(bw)
#y=395.686 - 25.017x1i

#(4)
sw <- stepAIC(fit,direction='both')

summary(sw)
#y=395.686 - 25.017x1i

#(5)
data <- data.frame(yi,x1i,x2i,x3i)
data

b <- regsubsets(yi~x1i+x2i+x3i,data=data) #각 크기에 대한 최선 모형
b
c_p <- summary(b)$cp
c_p

#c_p값이 가장 작은 p=1이 선택된다.

#(6)
R2ap <- summary(b)$adjr2
R2ap

#수정결정계수(R2ap)값이 최대인 p=2가 선택된다.

