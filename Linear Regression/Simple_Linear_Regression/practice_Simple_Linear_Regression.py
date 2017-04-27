# -*- coding: utf-8 -*-
"""
Created on Mon Apr 24 10:20:47 2017

@author: nekar01
"""

import numpy as np 
import matplotlib.pyplot as plt 
import pandas as pd 

#Need to understand more about the pNandas 
#Data Visualization is a great skill
dataset = pd.read_csv('Salary_Data.csv')
X = dataset.iloc[:,:-1].values
y = dataset.iloc[:,1].values

from sklearn.cross_validation import train_test_split
Xtrain,Xtest,ytrain,ytest = train_test_split(X,y,test_size=1/3,random_state = 0)

from sklearn.linear_model import LinearRegression
regressor = LinearRegression();
#trainig the regressor
regressor.fit(Xtrain,ytrain)

ypred = regressor.predict(Xtest)

# Visualizing the training set 
plt.scatter(Xtrain,ytrain,color="red")
plt.plot(Xtrain,regressor.predict(Xtrain),color ="blue")
plt.title('Graph for Training Site')
plt.xlabel('Experience_training Set')
plt.ylabel('Salary_trainig Set')
plt.show()


# Visualizing the test set 
plt.scatter(Xtest,ytest,color="red")
plt.plot(Xtrain,regressor.predict(Xtrain),color ="blue")
plt.title('Graph for Training Site')
plt.xlabel('Experience_training Set')
plt.ylabel('Salary_trainig Set')
plt.show()
