#!/usr/bin/env python
# coding: utf-8

# In[ ]:


# import pandas
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import re
import chart_studio.plotly as py


# In[ ]:


# Create the required data frames by reading in the files
#df = pd.read_excel("Data/SaleData.xlsx")
#df =pd.read_csv("Data/imdb.csv",escapechar='\\')
#df = pd.read_csv("Data/diamonds.csv")
#df1=pd.read_csv("Data/movie_metadata.csv")


# In[ ]:


# Q1 Find least sales amount for each item
def least_sales(df):
    least_sale = df.groupby("Item")["Sale_amt"].min()
    return least_sale


# In[ ]:


# Q2 compute total sales at each year X region
def sales_year_region(df):
    totSale = df.groupby(df["OrderDate"].apply(lambda x: x.year ))["Sale_amt"].sum()
    return totSale


# In[ ]:


# Q3 append column with no of days difference from present date to each order date
def days_diff(df):
    refDate=pd.Timestamp(input("Enter a date in format yy-mm--dd:"))
    df["days_diff"]=df["OrderDate"].apply(lambda x: abs(x-refDate))
    return df


# In[ ]:


# Q4 get dataframe with manager as first column and  salesman under them as lists in rows in second column.
def mgr_slsmn(df):
    list_managers_salesman = df.groupby('Manager')['SalesMan'].apply(set)
    return list_managers_salesman


# In[ ]:


# Q5 For all regions find number of salesman and number of units
def slsmn_units(df):
    region_salesman_sales = pd.DataFrame()
    region_salesman_sales["sales"] = df.groupby(["Region","SalesMan"])["Sale_amt"].sum().groupby("Region").sum()
    region_salesman_sales["salesman_cnt"] = df.groupby(["Region","SalesMan"])["Sale_amt"].count().groupby("Region").count()
    return region_salesman_sales


# In[ ]:


# Q6 Find total sales as percentage for each manager
def sales_pct(df):
    tot=df["Sale_amt"].sum()
    contribution = df.groupby('Manager')['Sale_amt'].sum().apply(lambda x:x/tot *100 )
    return contribution


# In[ ]:


# Q7 get imdb rating for fifth movie of dataframe
def fifth_movie(df):
    nth_movie=int(input("Enter the index to get movie and imdb rating:"))
    return df.loc[nth_movie-1,["title","imdbRating"]]


# In[ ]:


# Q8 return titles of movies with shortest and longest run time
def movies(df):
    return df[df["duration"] == df["duration"].min()][["title","duration"]],df[df["duration"] == df["duration"].max()][["title","duration"]]


# In[ ]:


# Q9 sort by two columns - release_date (earliest) and Imdb rating(highest to lowest)
def sort_df(df):
    return df.sort_values(by=["year","imdbRating"], ascending=[True,False])


# In[ ]:


# Q10 subset revenue more than 2 million and spent less than 1 million & duration between 30 mintues to 180 minutes
def subset_df(df):
    return df[df["duration"]>(30*60)][df["duration"]<(180*60)]


# In[ ]:


# Q11 count the duplicate rows of diamonds DataFrame.
def dupl_rows(df):
    return df.duplicated().count()


# In[ ]:


# Q12 droping those rows where any value in a row is missing in carat and cut columns
def drop_row(df):
    return df.dropna(subset=['carat', 'cut'])


# In[ ]:


# Q13 subset only numeric columns
def sub_numeric(df):
    return df.select_dtypes(include=['number'])


# In[ ]:


# Q14 compute volume as (x*y*z) when depth > 60 else 8
def volume(df):
    diamond=df.mask(df.eq('None')).dropna()
    diamond["Volume"]= diamond["x"]*diamond["y"]*(diamond["z"].astype("float32"))
    diamond["Volume"][diamond["depth"] <= 60] = 8
    return diamond


# In[ ]:


# Q15 impute missing price values with mean
def impute(df):
    return df.fillna(value=df.mean())


# In[ ]:


#BONUS QUESTIONS:

#Q1
#Generate a report that tracks the various Genere combinations for each type year on year.

def Bonus1(df):
    imdb1 = df.fillna(np.nan)
    imdb1 = imdb1.sort_values(by="year")
    df=imdb1.filter(['year','type','imdbRating','duration'], axis=1)
    imdb2=imdb1.iloc[:,17:46]
    str=""
    df["Genre_combo"] = (imdb2.apply(lambda x: x.index[x.astype(bool)].tolist() , 1)).apply(lambda x: str.join(x))
    df=df.groupby(['year','Genre_combo']).agg({'imdbRating':[min,max,np.mean],'duration':np.sum})
    return df


# In[ ]:


#Q1(ALTERNATE SOLUTION)
#Generate a report that tracks the various Genere combinations for each type year on year. The result
#data frame should contain type, Genere_combo, year, avg_rating, min_rating, max_rating,
#total_run_time_mins
def Bonus_1(df):
    imdb1 = df.fillna(np.nan)
    imdb1 = imdb1.sort_values(by="year")
    imdb2=imdb1.iloc[:,17:46]
    df=imdb1.filter(['year','type','imdbRating','duration'], axis=1)
    df['Genre_combo'] = imdb2.T.apply(lambda x: ''.join(x.index[x==1]),axis=0)
    df=df.groupby(['year','type','Genre_combo']).agg({'imdbRating':[min,max,np.mean],'duration':np.sum})
    return df


# In[ ]:


#Q2
#The results should contain year, min_length,
#max_length, num_videos_less_than25Percentile, num_videos_25_50Percentile ,
#num_videos_50_75Percentile, num_videos_greaterthan75Precentile

def Bonus2(df):
    df.dropna(inplace = True) 
    films=pd.DataFrame()
    films=df.filter(["imdbRating","year","duration"],axis=1)
    films['title_length']=df["wordsInTitle"].str.len()
    films["title_length"].corr(films["imdbRating"])
    quantile=pd.DataFrame(df["duration"].quantile([0.25,0.5,0.75]).reset_index())
    films["percentile"]=films["duration"].apply(lambda x:1 if x<quantile.iloc[0,1] else(2 if x<quantile.iloc[1,1] else (3 if x<quantile.iloc[2,1] else 4)))
    film_table=films.pivot_table(index="year",columns="percentile",values="title_length",aggfunc={"title_length":'count'},fill_value=0)
    film_table[["min",'max']]=films.groupby("year").agg({"title_length":[min,max]})
    return film_table


# In[ ]:


#Q3
#In diamonds data set Using the volumne calculated above, create bins that have equal population within
#them. Generate a report that contains cross tab between bins and cut. Represent the number under
#each cell as a percentage of total.

def Bonus3(df):
    diamond=df.mask(df.eq('None')).dropna()
    diamond["Volume"]= diamond["x"]*diamond["y"]*(diamond["z"].astype("float32"))
    diamond["Volume"][diamond["depth"] <= 60] = 8
    bins=(pd.qcut(list(diamond['Volume']),q=5,precision=1)).codes
    d=pd.CategoricalDtype(['1','2','3','4','5'],ordered=True)
    category=pd.Categorical.from_codes(bins,dtype=d)
    return pd.crosstab(diamond['cut'],category).apply(lambda r: r/r.sum(), axis=1)


# In[ ]:


#Q4.Generate a report that tracks the Avg. imdb rating quarter on quarter, in the last 10 years, for movies
#that are top performing. You can take the top 10% grossing movies every quarter. Add the number of top
#performing movies under each genere in the report as well.

def Bonus4(df1):
    df1.dropna(inplace=True)
    movie_grouped=df1.groupby(['title_year'])
    movie_top10=movie_grouped.apply(lambda x: x.sort_values(by='gross',ascending=False).head(1 if int(0.1*len(x))==0 else int(0.1*len(x)))).reset_index(drop=True)
    movie_top10_grouped=movie_top10.groupby(['title_year','genres'])
    movie_final=movie_top10_grouped.agg({"imdb_score":'mean'})
    movie_final["num of top movies"]=movie_top10_grouped.apply(lambda x:len(x))
    return movie_final


# In[ ]:


#Q5

def Bonus5(df):
    df["decile"]=pd.qcut(df["duration"],10,labels=False)
    x=df.groupby("decile")[["nrOfNominations","nrOfWins"]].sum()
    x["count"]=df.groupby("decile")["year"].count()
    y=df.iloc[:,np.r_[8,17:45]]
    z=y.groupby("decile")[y.columns.tolist()[1:28]].sum()
    z=z.transpose()
    e=pd.DataFrame(z.apply(lambda x: x.nlargest(3).index,axis=0).transpose(),)
    e.columns=["first","second","third"]
    x["top genres"]=e["first"]+","+e["second"]+","+e["third"]
    return x
