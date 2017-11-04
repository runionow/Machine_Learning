
# Simple Linear Regression

#install.packages("ISLR")
#install.packages("psych")
#install.packages("ggplot2")


require("ISLR")
require("psych")
require("ggplot2")

head(Auto)
names(Auto)
attach(Auto)

# lm Test.
lm.fit = lm(mpg~horsepower)
names(lm.fit)
summary(lm.fit)
confint(lm.fit)

# Simple Linear Regression.
n=1 

# Multivariate Linear Regression.+
# n = number of columns choosen to calculate y.

#initialization
theta = function(n){
  as.matrix(rep(0,n+1))
}

# predictor - horsepower
# response - mpg 

X = as.matrix(Auto$horsepower)
Y = as.matrix(Auto$mpg)
initTheta = theta(1)


plot(Auto$horsepower,Auto$mpg)

predict(lm.fit,data.frame(lstat=c(140,120)),interval ="prediction")
summary(Auto)

Auto1 = as.data.frame(Auto)

alpha = 0.000161
iterations = 400000

#iterations = 20

# Calculating Cost Function.
costFunction = function(initTheta,y,x){
  m = nrow(x)
  modX = as.matrix(cbind(rep(1,m),x))
  htheta = modX %*% initTheta
  cost = (colSums((htheta - y)^2))/(2*m) 
  cost
}

# Performing Gradient descent to reduce the cost function.
gradientDescent = function(initTheta,y,x,alpha,iterations){
  modX = as.matrix(cbind(rep(1,nrow(x)),x))
  iterationCurve = matrix(,ncol=4)
  colnames(iterationCurve)  = c("Iteration","Cost","theta0","theta1")
  m = nrow(x)
  for(it in 1:iterations)
  {
    iterationCurve = rbind(iterationCurve,c(it,costFunction(initTheta,y,x),as.vector(initTheta)))
    #print(it)
    #print(initTheta)
    htheta = modX %*% initTheta
    newPartialDiff = numeric()
    for(j in 1:ncol(modX)){
      newPartialDiff = c(newPartialDiff,(colSums((htheta - y)*as.matrix(modX[,j])))*alpha/m)
    }
    newPartialDiff = as.matrix(newPartialDiff)
    initTheta = initTheta-newPartialDiff
  }
  #iterationCurve = na.omit(iterationCurve)
  #print(iterationCurve)
  list("theta" = initTheta , "Cost.theta" = na.omit(iterationCurve))
}


theta = gradientDescent(initTheta,Y,X,alpha,iterations)
print(theta$theta)
#abline(lm.fit)
print(theta$Cost.theta)


#The closed form solution to a Linear Regression.
closed.form = solve(t(delta)%*%delta)%*%t(delta)%*%Y

p <- ggplot(Auto, aes(Auto$horsepower, Auto$mpg)) + geom_point()  + geom_abline(intercept =theta$theta[1,] ,slope = theta$theta[2,],colour = "red",size = 1) + labs(x="Horse Power" , y="mpg" , title = "Linear Regression" , caption = "based on Auto data available in ISLR package..")
v <- ggplot(as.data.frame(theta$Cost.theta), aes(theta0, theta1, z = Cost)) +stat_density2d(data = as.data.frame(theta$Cost.theta) ,geom="polygon", aes(fill = factor(Cost)),contour = TRUE,n=60)
print(p)
print(v)




