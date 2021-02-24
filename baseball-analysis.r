library(tidyverse)
library(magrittr)
library(readxl)
library(mgcv)
library(car)

baseball <- read_excel("Baseball.xlsx", 1)

baseball$swap <- (baseball$Team_1986 != baseball$Team_1987)

baseball$swap[which(baseball$swap == TRUE)] <- 1

baseball %<>% select(-Name,
                     -League_1986,
                     #-Div_1986,
                     -Team_1986, 
                     -League_1987, 
                     -Team_1987)

baseball$Salary %<>% as.numeric()

baseball %<>% select(Salary, Years, everything())

##### GLM analysis #####

pairs(~ log(Salary) + Years + AB_career, data=baseball)

## Test Years

plot(log(Salary) ~ Years, data = baseball)
years.glm <- glm(log(Salary) ~ poly(Years, 2, raw = TRUE), data = baseball)
test.years <- data.frame(Years = seq(min(baseball$Years), max(baseball$Years), 0.01))
predictedsalary <- predict(years.glm, test.years)
lines(test.years$Years, predictedsalary)

## Test AB

plot(log(Salary) ~ AB_career, data = baseball)
AB.glm <- glm(log(Salary) ~ poly(AB_career, 2, raw = TRUE), data = baseball)
AB.glm.2 <- glm(log(Salary) ~ I(log(AB_career)), data = baseball)
test.AB <- data.frame(AB_career = seq(min(baseball$AB_career), max(baseball$AB_career), 1))
predictedsalary <- predict(AB.glm, test.AB)
predictedsalary2 <- predict(AB.glm.2, test.AB)
lines(test.AB$AB_career, predictedsalary)
lines(test.AB$AB_career, predictedsalary2)

## Test interaction variables

pairs(~ AB_career + H_career + RBI_career + W_career + HR_career, data = baseball)
pairs(~ Years + H_career + RBI_career + W_career + HR_career, data = baseball)
pairs(~ log(Salary) + H_career + RBI_career + W_career + HR_career, data = baseball)

pairs(~ AB_1986 + H_1986 + HR_1986 + R_1986 + RBI_1986 + BB_1986, data = baseball)

# Remove outliers
baseball %<>% filter(HR_1986 < 100, H_career < 4000)

## Test changing teams

plot(Salary ~ swap, data = baseball)
# not useful

## New variables

baseball %<>%
  mutate(H_1986_prop = H_1986/AB_1986, 
         HR_1986_prop = HR_1986/AB_1986,
         R_1986_prop = R_1986/AB_1986,
         RBI_1986_prop = RBI_1986/AB_1986,
         BB_1986_prop = BB_1986/AB_1986,
         BA_1986 = H_1986_prop + BB_1986_prop,
         H_career_prop = H_career/AB_career, 
         HR_career_prop = HR_career/AB_career,
         R_career_prop = R_career/AB_career,
         RBI_career_prop = RBI_career/AB_career)

## Test 1986 variables

test <- glm(log(Salary) ~ H_1986_prop + HR_1986_prop + RBI_1986_prop + R_1986_prop + BB_1986_prop,
    data = baseball)

test <- glm(log(Salary) ~ H_1986_prop + BB_1986_prop,
            data = baseball)

summary(test)

vif(test)

## Test career variables

pairs(~ Years + H_career_prop + RBI_career_prop + W_career + HR_career_prop, data = baseball)

test_2 <- glm(log(Salary) ~ poly(Years, 2, raw = TRUE) + H_career_prop + RBI_career_prop + W_career + HR_career_prop,
              data = baseball)

test_2 <- glm(log(Salary) ~ poly(Years, 2, raw = TRUE) + H_career_prop + I(W_career/Years),
              data = baseball)

summary(test_2)

vif(test_2)

## Other variables

test_3 <- lm(log(Salary) ~ Position_1986, data = baseball)
summary(test_3)

test_4 <- glm(log(Salary) ~ Put_outs_1986 + Assists_1986 + Errors_1986, data = baseball)
summary(test_4)
vif(test_4)

test_5 <- glm(log(Salary) ~ Div_1986, data = baseball)
summary(test_5)

## Final model

# Included all significant variables
glm.salary <- glm(log(Salary) ~ 
                    H_1986_prop +
                    BB_1986_prop +
                    poly(Years, 2, raw = TRUE) + 
                    H_career_prop + 
                    I(W_career/Years) + 
                    Put_outs_1986 +
                    Assists_1986 + 
                    Errors_1986 +
                    Div_1986,
                  data = baseball[-274,]) # Check 274 for outlier
summary(glm.salary)
vif(glm.salary)

# Stepwise function to optimise model
glm.final <- step(glm.salary, direction = "both")
summary(glm.final)
# Remove insignificant variable (error)
glm.final <- glm(log(Salary) ~ 
                   BB_1986_prop + 
                   poly(Years, 2, raw = TRUE) + 
                   H_career_prop + 
                   I(W_career/Years) + 
                   Put_outs_1986, 
                 data = baseball[-274, ])
summary(glm.final)
vif(glm.final)

par(mfrow=c(2,2))
plot(glm.final)

par(mfrow=c(1,1))
predicted.salary.final <- predict(glm.final, baseball[-274,])
plot(predicted.salary.final, log(baseball$Salary[-274]))
plot(exp(predicted.salary.final), baseball$Salary[-274])
x <- seq(0, 2500, 100)
y <- x
abline(lm(y ~ x), col = "red")
abline(lm(exp(predicted.salary.final) ~ baseball$Salary[-274]), col = "blue")

# Question: Why is coefficient for proportion of walks to at bats negative?
# Seems to be somewhat correlated to overall wins
# Might be able to explain that higher proportion of walks doesn't necessarily
# Suggest better performance by player

##### Gam analysis #####

## Persp plot for AB_1986 and H_1986

grid <- list(AB_1986 = seq(from = 33, to = 687, length = 200),
             H_1986 = seq(from = 6, to = 238, length = 200))

baseball.gam <- gam(log(Salary) ~ s(AB_1986) + s(H_1986), data = baseball)
baseball.pr <- mgcv::predict.gam(baseball.gam, newdata = expand.grid(grid))
baseball.pr <- matrix(baseball.pr, nrow = 200, ncol = 200)
persp(grid$AB_1986, grid$H_1986, baseball.pr, xlab = "AB_1986", ylab = "H_1986",
      zlab = "log(Salary)", theta = -45, phi = 15, d = 2.0, tick = "detailed")

grid <- list(AB_career = seq(from = 33, to = 9778, length = 200),
             Years = seq(from = 0, to = 25, length = 200))

baseball.gam <- gam(log(Salary) ~ s(AB_career) + s(Years), data = baseball)
baseball.pr <- mgcv::predict.gam(baseball.gam, newdata = expand.grid(grid))
baseball.pr <- matrix(baseball.pr, nrow = 200, ncol = 200)
persp(grid$AB_career, grid$Years, baseball.pr, xlab = "AB_career", ylab = "Years",
      zlab = "log(Salary)", theta = -35, phi = 15, d = 5.0, tick = "detailed")

# Given that so many predictors are independent, not worthwhile to try generalised
# additive model

