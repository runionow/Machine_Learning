
# Multi Variate Linear Regression

#install.packages("ISLR")
#install.packages("psych")
#install.packages("ggplot2")


require("ISLR")
require("psych")
require("ggplot2")

head(Auto)
names(Auto)


# lm Test.
lm.fit = lm( data = Auto,mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin)
print(lm.fit$coefficients)
coeff = as.matrix(lm.fit$coefficients)
# names(lm.fit)
# summary(lm.fit)
# confint(lm.fit)

# Multivariate Linear Regression.+
# n = number of columns choosen to calculate y.

#initialization
theta = function(n){
  as.matrix(rep(0,n+1))
}

# predictor - all the columns except mpg
# response - mpg 

plot(Auto$horsepower,Auto$mpg)

summary(Auto)

Means.X = colMeans(Auto[,2:8]) 
Sd.X = apply(Auto[,2:8],2,sd)


Auto1 = as.data.frame(Auto[,1:8])
for( i in 2:ncol(Auto1)){
  Auto1[,i] = (Auto1[,i]-mean(Auto1[,i]))/sd(Auto1[,i])
}

y = as.matrix(Auto1[,1])

x = Auto1[,2:ncol(Auto1)]

n= ncol(x)


alpha = 0.3
#iterations = 400000
initTheta = theta(n)

iterations = 100

# Calculating Cost Function.
costFunction = function(initTheta,y,x){
  m = nrow(x)
  modX = as.matrix(cbind(rep(1,m),x))
  htheta = modX %*% initTheta
  cost = (colSums((htheta - y)^2))/(2*m) 
  cost
}

costFunction(initTheta,y,x)

# Performing Gradient descent to reduce the cost function.
gradientDescent = function(initTheta,y,x,alpha,iterations){
  modX = as.matrix(cbind(rep(1,nrow(x)),x))
  iterationCurve = matrix(,ncol=10)
  colnames(iterationCurve)  = c("Iteration","Cost","theta0","theta1","theta2","theta3","theta4","theta5","theta6","theta7")
  m = nrow(x)
  for(it in 1:iterations)
  { 
    iterationCurve = rbind(iterationCurve,c(it,costFunction(initTheta,y,x),as.vector(initTheta)))
    #print(it)
    print(initTheta)
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


theta = gradientDescent(initTheta,y,x,alpha,iterations)
print(theta$theta)
#abline(lm.fit)

print(theta$Cost.theta)

iterationCurve = ggplot( ) + geom_line(data = as.data.frame(theta$Cost.theta), aes(Iteration,Cost),stat = "identity" )+scale_y_continuous(breaks = NULL,labels = factor(theta$Cost.theta[,2])) +ggtitle(" Cost and Iteration Curve for Alpha = 0.003")
print(iterationCurve)


p <- ggplot(Auto, aes(Auto$horsepower, Auto$mpg)) + geom_point()  + geom_abline(intercept =theta$theta[1,] ,slope = theta$theta[2,],colour = "red",size = 1) + labs(x="Horse Power" , y="mpg" , title = "Linear Regression" , caption = "based on Auto data available in ISLR package..")
v <- ggplot(as.data.frame(theta$Cost.theta), aes(theta0, theta1, z = Cost)) +stat_density2d(data = as.data.frame(theta$Cost.theta) ,geom="polygon", aes(fill = factor(Cost)),contour = TRUE,n=60)
print(p)
print(v)


test.set = data.frame(cylinders=4,displacement=300,horsepower=200,weight=3500,acceleration=11,year=70,origin=2)

test.set1 = matrix(c(4,300,200,3500,11,70,2),ncol=1)
normalized.coff = matrix(c(1,(test.set1-Means.X)/Sd.X),ncol=1)

t(normalized.coff) %*% normalized.coff


test.set1 =matrix(test.set1 ,nrow = 1)

t(coeff) %*% t(test.set1)

delta = as.matrix(cbind(rep(1,nrow(Auto)),Auto[,2:8]))
Y = as.matrix(Auto[,1])
predict(lm.fit,test.set,type = "response" )
closed.form = solve(t(delta)%*%delta)%*%t(delta)%*%Y

rambo = as.data.frame(theta$Cost.theta)
p <- ggplot(rambo, aes(rambo$Iteration, rambo$Cost)) +geom_line()+ggtitle("Alpha = 0.3 Iterations = 100")+xlab("Iteration")+ylab("Cost") + scale_y_continuous(breaks = NULL)
print(p)
plot(x = theta$Cost.theta[,1],y = theta$Cost.theta[,2]) 

print(lm.fit$coefficients)
print(theta$theta)