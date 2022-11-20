#Basic regression

setwd("~/Projects/h20")
regdata <- read.csv("data/regdata.csv")
reg2data <- read.csv("data/regdata2.csv")

j = reg2data[,-1]
j['a2'] <- j$A**2
j['b2'] <- j$B**2
j['c2'] <- j$C**2

k <- log(j+1)

x <- regdata[,-1]

pairs(output ~ A + B + C, data=x, col = c("blue"),
      lower.panel = NULL)

cor(x)

#x <- scale(x, center=TRUE, scale=FALSE)
#x <- as.data.frame(x)

x['a2'] <- x$A**2
x['b2'] <- x$B**2
x['c2'] <- x$C**2

#x['exp_output'] = exp(x$output)

#92.64% variance before taking the log!
#97.71% WITH LOG!!

y <- log(x+1)

lm0 <- lm(output ~ log(A)+log(B)+log(C),data=regdata)
summary (lm0)

lm1 <- lm(output ~ A + B + C + A:B + A:C + B:C + A:B:C, data = x)


lm2 <- lm(output ~ a2 + b2 + c2 + A + B + C + A:B + A:C + B:C + A:B:C + a2:b2 + a2:c2 + b2:c2 + a2:b2:c2
          , data = x)
lm6 <- lm(output ~ a2 + b2 + c2 + A + B + C + A:B + A:C + B:C + A:B:C + a2:b2 + a2:c2 + b2:c2 + a2:b2:c2
          , data = y)

lm3 <- lm(output ~ a2 + b2 + c2 + a2:b2 + a2:c2 + b2:c2 + a2:b2:c2, data = x)


lm4 <- lm(output ~ a2 + b2 + c2 + A + B + C + A:B + A:C + B:C + A:B:C ,data = x)

lm5 <- lm(exp_output ~ a2 + b2 + c2 + A + B + C + A:B + A:C + B:C + A:B:C + a2:b2 + a2:c2 + b2:c2 + a2:b2:c2
          , data = x)

summary(lm6)

anova(lm2,lm4)

#plot predicted vs. actual values
plot(x=predict(lm6), y=k$output, col="blue",
     xlab='Log Predicted Values',
     ylab='Log Actual Values',
     main='Log Predicted vs.Log Actual Values'
    )
abline(a=0, b=1,col="red")


v = c(64,14,31) #set at half the minimum, you get the intercept. A practical floor.
v = c(50,50,50)
e = runif(3)
o = (log10(v[2])+e[1])/v[1]**2 + (log10(v[3])+e[2])/v[2]**2 + (log10(v[1])+e[3])/v[3]**2
o = -10*log10(o)
o

p = lm1$coefficients[1] + lm1$coefficients[2]*v[1] + lm1$coefficients[3]*v[2] + lm1$coefficients[4]*v[3]
p = p + lm1$coefficients[5]*v[1]*v[2] + lm1$coefficients[6]*v[1]*v[3] + lm1$coefficients[7]*v[3]*v[2]
p = p + lm1$coefficients[8]*v[1]*v[2]*v[3]
p

k <- k[1:50,]

k['predictions'] <- predict.lm(lm6, k)

plot(k$predictions,k$output,col="black",pch=20,
     xlab="Log Predicted",
     ylab = "Log Actual",
     main = "Predicting new data with our model")
abline(a=0, b=1,col="red")

k['o_output'] = exp(k['output'])
k['o_pred'] = exp(k['predictions'])
head(k)  
(cor(k['output'],k['predictions']))^2

plot(k$o_pred,k$o_output,col="black",pch=20,
     xlab="Predicted",
     ylab = "Actual",
     main = "Predicting new data with our model")
abline(a=0, b=1,col="red")
