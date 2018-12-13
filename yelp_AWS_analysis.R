library(DBI)
library(RMySQL)
library(dplyr)

# Are users more likely to follow elite users, as compared to non-elite users?

# The “dbConnect” command is to connect R studio with AWS Server, 
# so as to retrive the database from AWS Server. The parameters, 
# user name, password, dbname, host address and port are set from AWS.

mydb = dbConnect(MySQL(),host="34.219.51.92",dbname="yelp_db",user="user",password="msba_2018")

# After connecting R with the database "yelp_db" on AWS, 
# checking the tables inside with "dbListTables"
dbListTables(mydb)

# elite or not
# Extracting elites’ users in 2017
res = dbSendQuery(mydb, "select distinct user_id from elite_years where year = '2017'")
elite_2017 = fetch(res, n=-1)

dbClearResult(res)

# elite in 2016 or not
# Extracting elites’ users in 2016
res = dbSendQuery(mydb, "select distinct user_id from elite_years where year = '2016'")
elite_2016 = fetch(res, n=-1)

dbClearResult(res)

# elite times
# Calculating the times that a user was awarded as elite in history
res = dbSendQuery(mydb, "select user_id, year from elite_years")
elite = fetch(res, n=-1)
dbClearResult(res)

elite$year = 1
elite_count<-elite %>%
  group_by(user_id) %>%
  summarise(elite_times = sum(year))
dbClearResult(res)

# Calcuting the number of friends each user has.
res = dbSendQuery(mydb, "select user_id, count(1) from friend group by user_id")
num_friend = fetch(res, -1)
dbClearResult(res)

names(num_friend)[2] = "cnt_friends"

# review_cnt, average_star and useful
# Selecting the number of reviews users made, 
# average star-ratings and the number of useful comments
res = dbSendQuery(mydb, "select id, review_count, average_stars, useful from user group by id")
user = fetch(res, n=-1)
dbClearResult(res)

names(user)[1] = 'user_id'
# photo_cnt
# Calculating the number of photos users post 
res = dbSendQuery(mydb, "select id, label from photo group by id")
photo_cnt = fetch(res, n=-1)
dbClearResult(res)

photo_cnt$label = 1
library(dplyr)
photo_count<-photo_cnt %>%
  group_by(id) %>%
  summarise(count = sum(label))
# From the result of photo_count, 
# we find out that each distinct user posts only 1 photo through the Yelp. 
# There is no any data distribution regarding the number of photos users post. 
# Therefore, we decided not to apply variable photo_cnt into our prediction model.

# tip_cnt
# Calculating the number of tips users give
res = dbSendQuery(mydb, "select user_id, likes from tip group by id")
tip_cnt = fetch(res, n=-1)
dbClearResult(res)

tip_cnt$likes = 1
library(dplyr)
tip_count<-tip_cnt %>%
  group_by(user_id) %>%
  summarise(tip_count = sum(likes))

# Merging all required variables (is_elite 2017, is_elite_before 2016, 
# elite_count, user, tip_count) into one data frame, we called it “df”.
df = num_friend

df$is_elite = 0
df$is_elite[df$user_id %in% elite_2017$user_id] = 1

df$is_elite_before = 0
df$is_elite_before[df$user_id %in% elite_2016$user_id] = 1

df<- merge(df,elite_count,by='user_id',all.x=TRUE)

df<- merge(df,user,by='user_id',all.x=TRUE)

df<- merge(df,tip_count,by='user_id',all.x=TRUE)

df[is.na(df)]<-0

head(df)

# Visualization by boxplot

boxplot(df$cnt_friends ~ df$is_elite, df, ylim = c(0, 1000))
boxplot(df$cnt_friends ~ df$elite_times, df, ylim = c(0, 8000))
boxplot(df$cnt_friends ~ df$is_elite_before, df, ylim = c(0, 1000))

# Visualization by scatter plot
plot(y=df$cnt_friends, x=df$review_count,
     col="green",
     xlim=c(0,4000),
     ylim=c(0, 10000), 
     main="Relationship Btw cnt_friends and review_cnt",
     ylab="friends_count", xlab="review_cnt")

plot(y=df$cnt_friends, x=df$tip_count,
     col="blue",
     xlim=c(0,1000),
     ylim=c(0, 10000), 
     main="Relationship Btw cnt_friends and tip_count",
     ylab="friends_count", xlab="tip_count")

plot(y=df$cnt_friends, x=df$useful,
     col="orange",
     xlim=c(0,5000),
     ylim=c(0, 10000), 
     main="Relationship Btw cnt_friends and useful",
     ylab="friends_count", xlab="useful")

plot(y=df$cnt_friends, df$average_star,
     col="red",
     ylim=c(0, 10000), 
     main="Relationship Btw cnt_friends and avg_star",
     ylab="friends_count", xlab="avg_star")

# use poisson regression to build regression model
fit1 = glm(cnt_friends ~ is_elite + is_elite_before + elite_times + review_count 
              + average_stars + useful + tip_count, data = df, family = poisson)
summary(fit1)

# use correlation function to find relationship between variables, 
# in order to simplify variables.
s_data<-subset(df,select=c(is_elite, is_elite_before, elite_times, review_count, 
                           average_stars, useful, tip_count))
library('corrplot') 
M<-cor(s_data)
corrplot(M, method = "circle") 

# After analyzing the matrix above, we decide to use the following 
# independent variables for the regression model:
fit2 = glm(cnt_friends ~ is_elite  + elite_times  
           + average_stars + useful + tip_count, data = df, family = poisson)
summary(fit2)

# Explain the meaning of coefficient by exp(coefficient)
exp(fit1$coefficients) - 1
#------------------------------------------------------------------------------------------------
# Predict customer volumes (check-in) for the businesses with selected variables.
# Calculating the number of check-ins from table “checkin” 
# checkin count
res = dbSendQuery(mydb, "select business_id, count from checkin")
checkin = fetch(res, n=-1)
dbClearResult(res)

checkin_count<-checkin %>%
  group_by(business_id) %>%
  summarise(checkin_count = sum(count))

# Selecting neighborhood, state, review_count and stars of the business
res = dbSendQuery(mydb, "select id, state, neighborhood, review_count, stars from business")
business = fetch(res,n=-1)
dbClearResult(res)

names(business)[1] = 'business_id'

# Classifying neighborhood into “downtown” (1) or not (0), “nb” represents “neighborhood”
business$nb_downtown = 0
business$nb_downtown[business$neighborhood =='Downtown' | 
                       business$neighborhood =="Downtown Core"] =1
business$neighborhood<-NULL

# merge business
checkin_cnt<-merge(checkin_count,business,by="business_id")

# category 
res = dbSendQuery(mydb, "select business_id, category from category")
category = fetch(res,n=-1)
dbClearResult(res)

category$restaurant = 0
category$restaurant [category$category=="Restaurants"] = 1
category$category<-NULL

category<-category %>%
  group_by(business_id) %>%
  summarise(restaurant=sum(restaurant))

# Classifying attribute “ByAppointmetOnly” into “yes” (1) or no (0).
# Classifying attribute “BusinessAcceptsCreditCards” into “yes” (1) or no (0).
checkin_cnt<-merge(checkin_cnt,category,by="business_id", all.x=TRUE)

#attribute 'ByAppointmentOnly'
res = dbSendQuery(mydb, "select business_id from attribute 
                  where attribute.name = 'ByAppointmentOnly' and attribute.value = 1 ")
AppointmentOnly = fetch(res,n=-1)
dbClearResult(res)

# attribute 'BusinessAcceptsCreditCards'
res = dbSendQuery(mydb, "select business_id from attribute 
                  where attribute.name = 'BusinessAcceptsCreditCards' and attribute.value = 1 ")
CredictCard = fetch(res,n=-1)
dbClearResult(res)

# merge attributes
checkin_cnt$ByAppointmentOnly = 0
checkin_cnt$ByAppointmentOnly [checkin_cnt$business_id %in% AppointmentOnly$business_id] = 1

checkin_cnt$AcceptsCredictCard = 0
checkin_cnt$AcceptsCredictCard [checkin_cnt$business_id %in% CredictCard$business_id] = 1

# Converting variable “state” (categorical data) into binary values. 
# My logic is to count the number of distinct states, 
# and find out which state has the largest number. 
# For example, if state “CA” has the largest count, 
# we will create a new variable called “state_CA”. 
# Then, classifying all states into “state_CA” (1) or not (0).
state = checkin_cnt
state$state_time = 1
state_order<-state %>%
  group_by(state) %>%
  summarise(count = sum(state_time))

# merge state_AZ
checkin_cnt$state_AZ = 0
checkin_cnt$state_AZ [checkin_cnt$state == "AZ"] = 1

checkin_cnt$state<-NULL

head(checkin_cnt)
# Visualization boxplot
boxplot(checkin_cnt$checkin_count ~ checkin_cnt$AcceptsCredictCard, checkin_cnt, ylim = c(0,200))
boxplot(checkin_cnt$checkin_count ~ checkin_cnt$stars, checkin_cnt, ylim = c(0,200))
boxplot(checkin_cnt$checkin_count ~ checkin_cnt$nb_downtown, checkin_cnt, ylim = c(0,200))
boxplot(checkin_cnt$checkin_count ~ checkin_cnt$restaurant, checkin_cnt, ylim = c(0,200))
boxplot(checkin_cnt$checkin_count ~ checkin_cnt$ByAppointmentOnly, checkin_cnt, ylim = c(0,200))
boxplot(checkin_cnt$checkin_count ~ checkin_cnt$state_AZ, checkin_cnt, ylim = c(0,200))

# Visualization scatter plot
plot(y=checkin_cnt$checkin_count, x=checkin_cnt$review_count,
     col="red",
     xlim = c(0,2000),
     ylim=c(0, 6000), 
     main="Relationship Btw checkin_cnt and review_count",
     ylab="checkin_cnt", xlab="review_cnt")

# use poisson regression to build regression model
fit3 = glm(checkin_count ~ review_count + stars + nb_downtown + restaurant 
              + ByAppointmentOnly + state_AZ + AcceptsCredictCard, data = checkin_cnt, family = poisson)
summary(fit3)

#use correlation function to find relationship between variables, in order to simplify variables.
s_data2<-subset(checkin_cnt,select=c(review_count, stars, nb_downtown 
                          , ByAppointmentOnly, state_AZ, AcceptsCredictCard))
library('corrplot') 
M<-cor(s_data2)
corrplot(M, method = "circle") 

exp(fit3$coefficients) - 1
