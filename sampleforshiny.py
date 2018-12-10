
import pandas as pd
filepath = '201806-citibike-tripdata.csv'
df = pd.read_csv(filepath)
# print(df.columns)
df.drop(['stoptime','end station id','end station latitude','end station longitude','bikeid','birth year'],axis=1,inplace = True)

# print(df.columns)
df2 = df.sample(frac=0.05)
df2.to_csv('data_201806.csv')
