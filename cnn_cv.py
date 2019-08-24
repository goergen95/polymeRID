#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 24 13:34:47 2019

@author: marus200
"""

import keras as ks
from keras.utils import plot_model
import tensorflow as tf
import pandas as pd
from sklearn.model_selection import train_test_split, StratifiedKFold

def load_data_kfold(k):
    
    train = pd.read_csv("website/ref/reference_database.csv")       
    x_train = data.iloc[:, 0:1863]
    x_train = ks.utils.normalize(x_train, axis = 1)
    x_train = ks.backend.expand_dims(x_train, axis = -1)
    x_train = ks.backend.eval(x_train)
                         
    y_train = data.iloc[:, 1863]
    y_train[y_train == "FUR"] = 0
    y_train[y_train == "WOOD"] = 1
    y_train[y_train == "FIBRE"] = 2
    y_train[y_train == "PE"] = 3
    y_train[y_train == "PES"] = 4
    y_train[y_train == "PA"] = 5
    y_train[y_train == "PS"] = 6
    y_train[y_train == "HDPE"] = 7
    y_train[y_train == "PET"] = 8
    y_train[y_train == "PP"] = 9
    y_train[y_train == "LDPE"] = 10
    y_train[y_train == "PUR"] = 11
    y_train = pd.to_numeric(y_train)
    
    folds = list(StratifiedKFold(n_splits=k, shuffle=True, random_state=1).split(x_train, y_train))
    
    return folds, x_train, y_train

k = 10
folds, x_train, y_train = load_data_kfold(k)

def getmodel():
    model = ks.models.Sequential()
    # block 1
    model.add(ks.layers.Conv1D(filters = 8,
                               kernel_size= 70,
                               input_shape = (1863,1),
                               activation = "relu",
                               name= "conv1"))
    model.add(ks.layers.Conv1D(filters = 16,
                               kernel_size= 70,
                               activation = "relu",
                               name="conv2"))
    model.add(ks.layers.MaxPooling1D(strides = 2,
                                     pool_size= 5,
                                     name="pool1"))
    
    # block 2
    model.add(ks.layers.Conv1D(filters = 32,
                               kernel_size= 70,
                               activation = "relu",
                               name="conv3"))
    model.add(ks.layers.Conv1D(filters = 64,
                               kernel_size= 70,
                               activation = "relu",
                               name="conv4"))
    model.add(ks.layers.MaxPooling1D(strides = 2,
                                     pool_size= 5,
                                     name="pool2"))
    # exit block
    model.add(ks.layers.GlobalMaxPool1D(name="maxpool"))
    model.add(ks.layers.Dropout(rate = .5,name="dropout"))
    model.add(ks.layers.Dense(units = 12, 
                              activation= "softmax",
                              name="exit"))
    
    model.compile(loss="categorical_crossentropy",
                  optimizer="adam",
                  metrics=['accuracy'])
    
    plot_model(model, to_file="model_plot.png")
    
    return model

losses = list()
accuracies = list()
for j, (train_idx, val_idx) in enumerate(folds):
    TBcallback = ks.callbacks.TensorBoard(log_dir = "website/output/log"+str(j),
                                      batch_size=10,
                                      write_grads=True,
                                      write_graph=True,
                                      write_images=True)
    print('\nFold ',j)
    X_train_cv = x_train[train_idx]
    y_train_cv = y_train[train_idx]
    y_train_cv = ks.utils.to_categorical(y_train_cv.values, dtype = "int64")
    X_valid_cv = x_train[val_idx]
    y_valid_cv= y_train[val_idx]
    y_valid_cv =  ks.utils.to_categorical(y_valid_cv.values, dtype = "int64")

    
    model = getmodel()
    
    model.fit(x=X_train_cv, y=y_train_cv,
              batch_size = 10,
              epochs=300,
              callbacks=[TBcallback])
    print(model.evaluate(X_valid_cv,y_valid_cv))
    metrics = model.evaluate(X_valid_cv,y_valid_cv)
    losses.append(metrics[0])
    accuracies.append(metrics[1])

