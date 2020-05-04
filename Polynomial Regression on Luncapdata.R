
library("readxl")

data <- read_excel("LungCap.xls")


head(data)


?attach
attach(data)

summary(data)

colnames(data)

plot(Height,LungCap,main="Polynomial Reg",las = 1)

model1 <- lm(LungCap~Height)
summary(model1)


abline(model1,lwd=3,col="red")


# first WRONG way 
# This will ignore height^2
model2 <- lm(LungCap ~ Height + Height^2)
summary(model2)


# now the RIGHT way

model2 <- lm(LungCap ~ Height + I(Height^2))
summary(model2)
# Multiple R-squared:  0.7741 has increased
# Residual standard error: 1.238  has decreased




#Alternate way to do this

HeightSquare <- Height^2
model_new <- lm(LungCap ~ Height + HeightSquare)
summary(model_new)


# use poly 

model_new1 <- lm(LungCap ~ poly(Height,degree = 2,raw = T))
summary(model_new)

# if raw=F R will use orthogonal polynomials


lines(smooth.spline(Height,predict(model2)), col = "blue", lwd=3)

?anova
# compare the models

anova(model1,model2)

# null hypothesis is that there is no significant diff bw two models
#Alternative hypothesis is that poly model is significantly better

# very small p value so  (6.069e-14 ***)
# Rejest H0


# use Height^3

model3 <- lm(LungCap ~ Height + I(Height^2) + I(Height^3)) 
summary(model3)

# when u include h^3 in the model also include h and h^2

lines(smooth.spline(Height,predict(model3)),col = "green",lty = 3,lwd =3)

# adding legends

legend(46,15,legend = c("Model1 : Linear","Model2 : poly x^2","Model3 : poly x^3"),
       col = c("red","blue","green"),lty = c(1,1,3),lwd = 3,
       bty = "o",cex = 0.9
        )

anova(model2,model3)
# p value is  0.6582
# hence including Height^3 doesnot improve our model


