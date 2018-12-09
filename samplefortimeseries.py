#coding:utf-8

import os
import pandas as pd
filepaths = os.listdir('./data/')
data = []
for i in filepaths:
    if i.endswith(".csv"):
        df = pd.read_csv(i)
        df_sample = df.sample(frac = 0.05)
        data.append(df_sample)

results = pd.concat(data)
results.to_csv('data_2017_2018.csv')