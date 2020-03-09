#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Mar  7 15:38:34 2020

@author: sunkailai
"""

import gensim
import pandas as pd
import numpy as np
from gensim import corpora
import re
import nltk
from nltk import word_tokenize, sent_tokenize
from nltk.corpus import stopwords
from nltk.stem import PorterStemmer
import datetime

hairdryer = pd.read_csv('hair_dryer.csv')
pacifier = pd.read_csv('pacifier.csv')
microwave = pd.read_csv('microwave.csv')

def preprocessing(text):
    text = re.sub('[^\w ]','',str(text))
    #分词
    token_words = word_tokenize(text)
    #去除停用词
    stop_words = stopwords.words('english')
    fileter_words = [word for word in token_words if word not in stop_words]
    #去除无用词
    pos_tags = nltk.pos_tag(fileter_words)
    for word,pos in pos_tags:
        if (pos == 'CC' or pos == 'CD' or pos == 'EX' or pos == 'PRP' or pos == 'RP' or pos == 'TO' or pos == 'DT' or pos == 'IN' or pos == 'MD'):
            fileter_words.remove(word)
    #stemmer
    stemmer = PorterStemmer()
    fileterStem_words = [stemmer.stem(word) for word in fileter_words]
    
    return ' '.join(fileterStem_words) #返回一个字符串  以空格间隔



hairdryer_split = hairdryer['review_body'].apply(preprocessing)
pacifier_split = pacifier['review_body'].apply(preprocessing)
microwave_split = microwave['review_body'].apply(preprocessing)


rate_words_pacifier = {1:[], 2:[], 3:[], 4:[], 5:[]}
for i in range(18935):
    text = pacifier_split[i]
    words = re.split(' ', text)
    rate = int(pacifier.loc[i, 'star_rating'])
    rate_words_pacifier[rate] = rate_words_pacifier[rate] + words
    

rate_words_hairdryer = {1:[], 2:[], 3:[], 4:[], 5:[]}
for i in range(11470):
    text = hairdryer_split[i]
    words = re.split(' ', text)
    rate = int(hairdryer.loc[i, 'star_rating'])
    rate_words_hairdryer[rate] = rate_words_hairdryer[rate] + words

rate_words_microwave = {1:[], 2:[], 3:[], 4:[], 5:[]}
for i in range(1615):
    text = microwave_split[i]
    words = re.split(' ', text)
    rate = int(microwave.loc[i, 'star_rating'])
    rate_words_microwave[rate] = rate_words_microwave[rate] + words

rate_words_pacifier = pd.Series(rate_words_pacifier)
rate_words_hairdryer = pd.Series(rate_words_hairdryer)
rate_words_microwave = pd.Series(rate_words_microwave)
rate_words = rate_words_pacifier + rate_words_hairdryer + rate_words_microwave

rate_words_microwave_all = []
for i in range(1,6):
    rate_words_microwave_all = rate_words_microwave_all + rate_words_microwave[i]
pd.Series(rate_words_microwave_all).value_counts()[0:20].plot.bar(figsize=(9,6))
    


star_1 = pd.Series(rate_words[1]).value_counts()
fig = star_1[0:20].plot.bar(figsize=(9,6)).get_figure()
fig.savefig('star_1.png')

star_5 = pd.Series(rate_words[5]).value_counts()
fig = star_5[0:20].plot.bar(figsize=(9,6)).get_figure()
fig.savefig('star_5.png')

star_2 = pd.Series(rate_words[2]).value_counts()
fig = star_2[0:20].plot.bar(figsize=(9,6)).get_figure()
fig.savefig('star_2.png')

star_3 = pd.Series(rate_words[3]).value_counts()
fig = star_3[0:20].plot.bar(figsize=(9,6)).get_figure()
fig.savefig('star_3.png')

star_4 = pd.Series(rate_words[4]).value_counts()
fig = star_4[0:20].plot.bar(figsize=(9,6)).get_figure()
fig.savefig('star_4.png')


hairdryer_tb = pd.read_excel('hair_dryer_textblob.xlsx')
microwave_tb = pd.read_excel('microwave_textblob.xlsx')
pacifier_tb = pd.read_excel('pacifier_textblob.xlsx')
all_tb = pd.concat([hairdryer_tb, microwave_tb, pacifier_tb], axis = 0, ignore_index = True)
all_tb = all_tb.drop([0.1], axis = 1)
corr = all_tb.corr()

product_count = all_tb['product_id'].value_counts()
dic_all = all_tb.T.to_dict()


times = {}
for i in product_count.index.tolist():
    times[i] = [] 
for i in range(32020):
    print(i)
    product_id = dic_all[i]['product_id']
    s = product_count[product_id]
    time = dic_all[i]['review_date']
    time = datetime.datetime.strptime(time, '%m/%d/%Y')
    if time in times[product_id]:
        continue
    times[product_id].append(time)
    dic_all[i]['s'] = s
    
periods = {}
for k, v in times.items():
    if len(v) == 1:
        period = 1
    else:
        start = v[0]
        end = v[0]
        for i in v:
            diff_1 = (i - start).days
            diff_2 = (i - end).days
            if diff_1 < 0:
                start = i
                continue
            if diff_2 > 0:
                end = i
                continue
        period = (end - start).days
    periods[k] = period


for i in range(32020):
    product_id = dic_all[i]['product_id']
    t = periods[product_id]
    dic_all[i]['t'] = t     

all_tb = pd.DataFrame(dic_all).T
all_tb['d'] = all_tb['s'] / all_tb['t']
all_tb[all_tb['t'] != 1]['d'].plot.hist()


all_tb = pd.read_excel('all.xlsx')
corr = all_tb[all_tb['t'] != 1].corr()

import sklearn
from sklearn import datasets, linear_model
import matplotlib.pyplot as plt
import math
from sklearn.model_selection import  train_test_split
X = all_tb[all_tb['t'] != 1][['star_rating', 'polarity', 'subjectivity']]
y = all_tb[all_tb['t'] != 1]['d']
#y = y.apply(lambda x: math.exp(x))
X_train, X_test, y_train, y_test = train_test_split(X, y, random_state=1)

print(X_train.shape)
print(y_train.shape)
print(X_test.shape)
print(y_test.shape)

from sklearn.linear_model import LinearRegression
linreg = LinearRegression()
linreg.fit(X_train, y_train)

print(linreg.intercept_)
print(linreg.coef_)


all_tb.to_excel('all.xlsx')

from sklearn import ensemble
from sklearn.model_selection import cross_val_score
#集成学习随机森林RandomForestRegressor回归模型
def test_RandomForestRegressor(*data):
    X_train,X_test,y_train,y_test=data
    regr=ensemble.RandomForestRegressor(max_depth = 30, oob_score=True, n_estimators = 15)
    regr.fit(X_train,y_train)
    print("Traninging Score:%f"%regr.score(X_train,y_train))
    print("Testing Score:%f"%regr.score(X_test,y_test))
    importances = regr.feature_importances_
    indices = np.argsort(importances)[::-1]
    feat_labels = ['star_rating', 'polarity', 'subjectivity']
    for f in range(X_train.shape[1]):
        print("%2d) %-*s %f" % (f + 1, 30, feat_labels[indices[f]], importances[indices[f]]))

# 调用 test_RandomForestRegressor
test_RandomForestRegressor(X_train,X_test,y_train,y_test) 



def test_RandomForestRegressor_num(*data):
    '''
    测试 RandomForestRegressor 的预测性能随  n_estimators 参数的影响
    '''
    X_train,X_test,y_train,y_test=data
    nums=np.arange(1,20,step=1)
    fig=plt.figure()
    ax=fig.add_subplot(1,1,1)
    testing_scores=[]
    training_scores=[]
    for num in nums:
        regr=ensemble.RandomForestRegressor(n_estimators=num, oob_score=True)
        regr.fit(X_train,y_train)
        training_scores.append(regr.score(X_train,y_train))
        testing_scores.append(regr.score(X_test,y_test))
        print(num)
        print(regr.score(X_train,y_train))
        print(regr.score(X_test,y_test))
    ax.plot(nums,training_scores,label="Training Score")
    ax.plot(nums,testing_scores,label="Testing Score")
    ax.set_xlabel("estimator num")
    ax.set_ylabel("score")
    ax.legend(loc="lower right")
    ax.set_ylim(-1,1)
    plt.suptitle("RandomForestRegressor")
    plt.show()
    

    
# 调用 test_RandomForestRegressor_num
test_RandomForestRegressor_num(X_train,X_test,y_train,y_test) 


def test_RandomForestRegressor_max_depth(*data):
    '''
    测试 RandomForestRegressor 的预测性能随  max_depth 参数的影响
    '''
    X_train,X_test,y_train,y_test=data
    maxdepths=range(1,50)
    fig=plt.figure()
    ax=fig.add_subplot(1,1,1)
    testing_scores=[]
    training_scores=[]
    for max_depth in maxdepths:
        regr=ensemble.RandomForestRegressor(max_depth=max_depth, oob_score=True, n_estimators = 15)
        regr.fit(X_train,y_train)
        training_scores.append(regr.score(X_train,y_train))
        testing_scores.append(regr.score(X_test,y_test))
        print(max_depth)
        print(regr.score(X_train,y_train))
        print(regr.score(X_test,y_test))
    ax.plot(maxdepths,training_scores,label="Training Score")
    ax.plot(maxdepths,testing_scores,label="Testing Score")
    ax.set_xlabel("max_depth")
    ax.set_ylabel("score")
    ax.legend(loc="lower right")
    ax.set_ylim(-0.5,1.05)
    plt.suptitle("RandomForestRegressor")
    plt.show()
    
# 调用 test_RandomForestRegressor_max_depth
test_RandomForestRegressor_max_depth(X_train,X_test,y_train,y_test) 


def test_RandomForestRegressor_max_features(*data):
    '''
   测试 RandomForestRegressor 的预测性能随  max_features 参数的影响
    '''
    X_train,X_test,y_train,y_test=data
    max_features=np.linspace(0.01,1.0)
    fig=plt.figure()
    ax=fig.add_subplot(1,1,1)
    testing_scores=[]
    training_scores=[]
    for max_feature in max_features:
        regr=ensemble.RandomForestRegressor(max_features=max_feature, max_depth = 30, oob_score=True, n_estimators = 15)
        regr.fit(X_train,y_train)
        training_scores.append(regr.score(X_train,y_train))
        testing_scores.append(regr.score(X_test,y_test))
        print(max_feature)
        print(regr.score(X_train,y_train))
        print(regr.score(X_test,y_test))
    ax.plot(max_features,training_scores,label="Training Score")
    ax.plot(max_features,testing_scores,label="Testing Score")
    ax.set_xlabel("max_feature")
    ax.set_ylabel("score")
    ax.legend(loc="lower right")
    ax.set_ylim(-0.5,1.05)
    plt.suptitle("RandomForestRegressor")
    plt.show()
    
# 调用 test_RandomForestRegressor_max_features
test_RandomForestRegressor_max_features(X_train,X_test,y_train,y_test)
