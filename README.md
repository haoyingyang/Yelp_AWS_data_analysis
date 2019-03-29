# Yelp_AWS_data_analysis
This project is about Yelp customer and business behavior analysis, focusing on two major questions, first one is 'Are users more likely to follow elite users, as compared to non-elite users?', 'second one is Predict customer volumes (check-in) for the businesses with selected variables.' In this project, due to the huge size of the data, I utilized SQL to query data from AWS, and then use R to analyze date and data visualization. 

connect to data on AWS:
```{r}
library(DBI)
library(RMySQL)
mydb = dbConnect(MySQL(), user='remuser', password='rempwd', dbname='yelp_db', host='ec2-34-216-35-252.us-west-2.compute.amazonaws.com', port=3306)
```
