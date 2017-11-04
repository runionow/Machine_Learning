#(a)Implement a sigmoid function
sigmoid = function (z){
   1/(1+exp(-z))
}

# Function to standardize input values
zscore <- function(x, mean.val=NA) {
  if(is.matrix(x)) return(apply(x, 2, zscore, mean.val=mean.val))
  if(is.data.frame(x)) return(data.frame(apply(x, 2, zscore, mean.val=mean.val)))
  if(is.na(mean.val)) mean.val <- mean(x)
  sd.val <- sd(x)
  if(all(sd.val == 0)) return(x) # if all the values are the same
  (x - mean.val) / sd.val 
}


#initialization
theta = function(n){
  as.matrix(rep(0,n+1))
}


random.numbers= seq(-10,10,0.001)
plot(random.numbers, sigmoid(random.numbers), col='red',main = "Problem 3: Sigmoid Function",ylab="Sigmoid Function" , xlab = "X")

#(b) Implement Logistic Regression.

require("ISLR")

Auto1 = cbind(Auto)


check.median = function(z){
  med = median(Auto[,1])
  if(z>=med){
    1
  }
  else{
    0
  }
}

xx = Auto[,1]
mpg01 =c()
med = median(xx)
for (i in xx){
  if(i >= med){
    mpg01 = c(mpg01,1)
  }
  else{
    mpg01 = c(mpg01,0)
  }
}


Auto1 = cbind(mpg01,Auto)

fit.logistic.regression = glm(data = Auto1,formula = mpg01~cylinders+displacement+horsepower+weight)
new.data = data.frame(cylinders=8,displacement=340,horsepower=200,weight=3500)
predict(fit.logistic.regression,new.data,type = "response" )

y = Auto1[,1] 
x = zscore(Auto1[,3:6])

Means.X = colMeans(Auto1[,3:6])
Sd.X = apply(Auto1[,3:6],2,sd)

initTheta = theta(4)

# for( i in 1:ncol(x)){
#   x[,i] = (x[,i]-mean(x[,i]))/sd(x[,i])
# }


alpha = 1e-9
iterations = 10000
  
# Calculating Cost Function.
costFunction = function(initTheta,y,x){
  m = nrow(x)
  modX = as.matrix(cbind(rep(1,m),x))
  htheta = sigmoid(modX %*% initTheta)
  cost = (colSums((-y%*%log(htheta))-((1-y)%*%log(1-htheta))))/m
  cost
}

#Gradient Descent
gradientDescent = function(initTheta,y,x,alpha,iterations){
  modX = as.matrix(cbind(rep(1,nrow(x)),x))
  iterationCurve = matrix(,ncol=7)
  colnames(iterationCurve)  = c("Iteration","Cost","theta0","theta1","theta2","theta3","theta4")
  m = nrow(x)
  for(it in 1:iterations)
  {
    iterationCurve = rbind(iterationCurve,c(it,costFunction(initTheta,y,x),as.vector(initTheta)))
    #print(it)
    #print(initTheta)
    htheta = sigmoid(modX %*% initTheta)
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

compare.x = data.frame(as.data.frame(rep(1,nrow(Auto))),x)
final.x = sigmoid(as.matrix(compare.x) %*%  as.matrix(theta$theta))

final.x = apply(final.x, 1, function(x) {
  if(x >= 0.5){
    1
  }
  else{
    0
  }
})

final.x = as.matrix(final.x)


success = table(final.x == Auto1[,1])["TRUE"] 
failed =  table(final.x == Auto1[,1])["FALSE"]


predict.data = matrix(c(1,8,340,200,3500),nrow = 1)
sigmoid(predict.data %*% theta$theta)
cost = costFunction(initTheta,y,x)

test.set1 = matrix(c(8,340,200,3500),ncol=1)
normalized.coff = matrix(c(1,(test.set1-Means.X)/Sd.X),ncol=1)
sigmoid(t(normalized.coff) %*% theta$theta)

rambo = as.data.frame(theta$Cost.theta)
p <- ggplot(rambo, aes(rambo$Iteration, rambo$Cost)) +geom_line()+ggtitle("Alpha = 0.00003 Iterations = 100")+xlab("Iteration")+ylab("Cost") + scale_y_continuous(breaks = NULL)
print(p)
