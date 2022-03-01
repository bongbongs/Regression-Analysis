#연습문제 5.2
x <- c(1,1,1,2,2,2,4,4,4,8,8,8)
y <- c(13.5,15.4,16.1,18.2,19.6,20.2,21.8,22.2,23.1,23.6,24.7,24.9)

#(1)
plot(x,y)

#(2)
x_prime=log(x)
plot(x_prime,y)

#(3)
#2번 그래프가 선형에 더 가깝다.

#연습문제 5.3
#(1)
(Ex5_2.lm <- lm(y~x))
summary(Ex5_2.lm)

sum((Ex5_2.lm$residuals)^2)/10 #MSE

#(2)
(Ex5_2_1.lm <- lm(y~x_prime))

sum((Ex5_2_1.lm$residuals)^2)/10 #MSE

#(3)
#x에 대한 MSE가 log(x)에 대한 MSE보다 크므로 5.2의 (2)번 모형이 더 적합하다.

#연습문제 5.12
#(1)
x <- c(2,6,10)
y <- c(4,7,4)

(Ex5_12.lm <- lm(y~x))

(coeffs <- coefficients(Ex5_12.lm))
confint(Ex5_12.lm,level=0.90)

#B1의 90% 신뢰구간은 (-2.733935,2.733935)이다.


#연습문제 5.14
#(1)
x <- c(1:14)
y <- c(6.0,6.3,6.1,6.8,7.5,8.0,8.1,8.5,9.0,8.7,7.9,8.2,8.4,9.0)
plot(x,y)

#(2)
(Ex_5.14.lm <- lm(y~x))
Ex_5.14.lm$residuals

a <- Ex_5.14.lm$residuals[2:14]- Ex_5.14.lm$residuals[1:13]

a

plot(2:14,a)

#시간에 따라 값이 낮아졌다 높아졌다 하는것을 보아 자기상관이 있어보인다.

#(3)
#install.packages("lmtest")
library(lmtest)
(dwtest(Ex_5.14.lm,alternative="greater")) #d 검정통계량 : 0.62458

#d값이 0.62458로 부록을 참조한 (n=15, 유의수준=0.01, 독립변수의 수=1)인 d 하한값인 0.67보다 작으므로 귀무가설(자기상관=0)을 기각할 수 있다. 따라서 오차항 사이에 자기상관이 존재한다.

#(4)
#일치한다.

