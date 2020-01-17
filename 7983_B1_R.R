library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(varhandle)
library(readr)
library(stringr)
library(reshape2)
library(mltools)
library(imputeTS)
library(assertr)
library(descr)
#Q1-Q6 Using SaleData.xlsx

#excel_sheets('SaleData.xlsx')
#Sales<-read_excel('SaleData.xlsx')

#Q1.Find least sales amount for each item

q1 <-function(df){
least_sale_amt <- tapply(df$Sale_amt,df$Item,min)
print(least_sale_amt)
}

#Q2.compute total sales at each year X region

q2 <-function(df){
total_sale_year <- tapply(df$Sale_amt,year(df$OrderDate),sum)
print(total_sale_year)
}

#Q3.append column with no of days difference from present date to each order date

q3 <-function(df){
date <-readline("Enter date:")
df$days_diff <- round(difftime(date,df$OrderDate))
print(Sales)
}


#Q4.get dataframe with manager as first column and  salesman under them as lists in rows in second column.

q4 <-function(df){
return(lapply(split(df$SalesMan,df$Manager),unique))
}

#Q5.For all regions find number of salesman and number of units

q5 <-function(df){
grouped_sales<-df%>%group_by(Region)%>%summarise(n_distinct(SalesMan))
SaleAmt<-tapply(df$Sale_amt,df$Region,FUN=sum)
return(data.frame(grouped_sales,SaleAmt,row.names = NULL))
}

#Q6.Find total sales as percentage for each manager


q6 <-function(df){
percent_sales<- data.frame(sort(unique(df$Manager)),((tapply(df$Sale_amt,df$Manager,sum))/sum(df$Sale_amt))*100,row.names = NULL)
colnames(percent_sales)<- c("Manager","percent_sales")
print(percent_sales)
}


#Q7-Q10 Using imdb.csv

#df<- read.csv("imdb.csv")


# Q7 get imdb rating for fifth movie of dataframe

q7 <-function(df){
fifth_movie <- df[['imdbRating']][5]
print(fifth_movie)
}


#Q8 return titles of movies with shortest and longest run time

q8 <-function(df){
print("Movie with minimum duration")
df$title[which.min(df$duration)]
print("Movie with maximum duration")
df$title[which.max(df$duration)]
}


# Q9 sort by two columns - release_date (earliest) and Imdb rating(highest to lowest)

q9 <-function(df){
imdb <-df%>% drop_na(year,imdbRating)
order_imdb<-imdb[order( imdb[,9],imdb[,6] ),]
head(order_imdb)
}


# Q10 subset revenue more than 2 million and spent less than 1 million & duration between 30 mintues to 180 minutes

q10 <-function(df){
filter_by_duration <- df %>%filter(duration>1800,duration<180*60)
print(filter_by_duration)
}


#Q11-Q15 Using diamonds.csv


#diamonds <- read.csv("diamonds.csv")

# Q11 count the duplicate rows of diamonds DataFrame.

q11 <-function(df){
duplicate_count <- group_size(group_by(df))
print(duplicate_count)
}


# Q12 droping those rows where any value in a row is missing in carat and cut coluna_mns\

q12 <-function(df){
na_omitted_diamonds<-na.omit(df)
print(na_omitted_diamonds)
}


# 13. Subset the dataframe with only numeric columns.

q13 <-function(df){
numeric_diamonds <- select_if(df,is.numeric)
print(numeric_diamonds)
}


# 14. Compute volume as (x y z) when depth is greater than 60. In case of depth less than 60 default volume to 8. 

q14 <-function(df){
na_omitted_diamonds<-na.omit(df)
na_omitted_diamonds<-unfactor(na_omitted_diamonds)
na_omitted_diamonds$volume <- ifelse(na_omitted_diamonds$depth>60,(na_omitted_diamonds$x)*(na_omitted_diamonds$y)*(as.integer(na_omitted_diamonds$z)), 8)
print((na_omitted_diamonds))
}


# Q15 impute missing price values with mean

q15 <-function(df){
df$price[is.na(df$price)]<-mean(df$price,na.rm=T)
return(df)
}



#df=read_delim("imdb.csv",delim=',',escape_double=F,escape_backslash=T)
#df<-na.omit(df,cols="duration")
#diamonds<-read_delim("diamonds.csv", delim=',', escape_double=FALSE, escape_backslash=TRUE)
#movies<-read_delim("movie_metadata.csv",delim=',',escape_double=F,escape_backslash=T)
#movie<-na.omit(movies,cols="duration")


Bonus1<-function(df){
  
  dfGenre<-df %>% select(17:44)
  df['genre_combo'] <- apply(df %>% select(16:44), 1, function(x) paste(names(x[x==1]), collapse=" "))
  df1 <- df %>% group_by(year,type,genre_combo)%>% summarise(avg_rating=mean(imdbRating),min_rating=min(imdbRating),max_rating=max(imdbRating),total_run_time_mins=(sum(duration)/60))
  return(df1)
}



Bonus2<-function(df)
{ 
  x<-na_mean(df)
  x$year=floor(x$year)
  x$len=nchar(x$wordsInTitle)
  x[is.na(x$wordsInTitle),"len"]=nchar(as.character(unlist(x[is.na(x$wordsInTitle),"title"])))
  x$percentile<-bin_data(x$duration,bins=4,binType = "quantile")
  d<-as.data.frame.matrix(table(x$year,x$percentile))
  colnames(d)<-c("num_videos_less_than25Percentile","num_videos_25_50Percentile ","num_videos_50_75Percentile","num_videos_greaterthan75Precentile")
  y<-x%>%group_by(year)%>%summarise(min=min(len),max=max(len))
  print(cbind(y,d))
}


Bonus3<-function(df)
{
  df$z[df1$z=="None"]<-NA
  df$z<-as.numeric(df1$z)
  df2<-na_mean(df)
  df2$volume <- ifelse(df2$depth>60,(df2$x)*(df2$y)*(as.integer(df2$z)), 8)
  df2$quant <- as.numeric(bin_data(df2$volume,bins = 5,binType = 'quantile'))
  df3<-crosstab(df2$cut,df2$quant,plot=FALSE,type = c("f", "c"),prop.t=T)
  return(df3)
}


Bonus4<-function(df){
  
  movie<-df%>%group_by(title_year)
  movie<-movie[with(movie,order(-gross)),]
  movie<-movie%>% group_map(~head(.x,ifelse(nrow(.x)<10,1,as.numeric(0.1*nrow(.x)))),keep=T) %>%bind_rows()
  movie<-movie%>%group_by(title_year,genres)%>%summarise(AvgImdb=mean(imdb_score),count=n())
  return(movie)
}


Bonus5 <- function(df){
  df<-na.omit(df,cols="duration")
  df$decile <- as.numeric(bin_data(df$duration,bins=10,binType='quantile'))
  df2 <- df %>% group_by(decile)
  dfGenre<-df2 %>% select(17:44) %>% summarise_all(funs(sum))
  df3<-df2 %>% summarise(TotlaNomination=sum(nrOfNominations),TotalWins=sum(nrOfWins),counts=n())
  df3["TopGenre"]<-col_concat(t(as.data.frame(apply(dfGenre, 1, function(x) head(names(dfGenre)[order(-x)],3)), stringsAsFactors=FALSE)),sep=" ")
  return(df3)
}




















