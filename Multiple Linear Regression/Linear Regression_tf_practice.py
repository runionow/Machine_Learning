# -*- coding: utf-8 -*-
"""
Created on Tue Apr 25 10:59:33 2017

@author: nekar01
"""
# conda create -n tensorflow
# activate tensorflow

import tensorflow as tf 
import numpy as np 

import pylab 


# 1. Create Data using numpy
x_data = np.random.rand(100).astype(np.float32)
x_data_shaped = np.random.rand(100).astype(np.float32).reshape((25,4))
noise = np.random.normal(scale=0.01, size = len(x_data))
y_data = x_data * 0.1 +0.3+noise

pylab.plot(x_data,y_data,'.',color='red')

# 2. Build Inference Graph
# Create variables w and b that compute y_data = w*x_data +b

W = tf.Variable(tf.random_uniform([1],0.0,1.0),name = "Arun")
b = tf.Variable(tf.zeros([1]))
y = W*x_data + b 
print(W.name)
print(b.name)

# 3. Build Training Graph
loss = tf.reduce_mean(tf.square(y-y_data))
optimizer = tf.train.GradientDescentOptimizer(0.5)
train = optimizer.minimize(loss)
init = tf.initialize_all_variables()

print("loss :", loss)
print("optimizer :", optimizer)
print("train :", train)
print(init)


