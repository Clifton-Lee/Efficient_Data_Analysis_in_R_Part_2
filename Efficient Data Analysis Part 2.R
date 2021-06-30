
library(Amelia)
library(lubridate)
library(tidyverse)

## Data Understanding


dataset <- read_csv("sales2019.csv") # importing the dataset

dim(dataset) # dimensions of the datset

colnames(dataset) # column names 

glimpse(dataset) # data structure

missmap(dataset, col = c('black', 'grey'), margins = c(7.5,5)) #missing data map 

## Data Cleaning and Pre-processing 

dataset <- dataset %>% 
           filter(!is.na(user_submitted_review))

dim(dataset)

# average number of books purchaed in the total purchased column
dataset %>% 
  summarize(avg_purchased = mean(total_purchased,na.rm = TRUE))


# Impute the average number of books
dataset <- dataset %>% 
            mutate(total_purchased_complete = if_else(is.na(total_purchased),
                                                      round(mean(total_purchased,na.rm = TRUE),digits = 0),
                                                      total_purchased)) %>% 
            select(-total_purchased)

head(dataset)

unique(dataset$user_submitted_review) #examine the unique sentences in the column

pos_neg <- function(string){
  case_when(str_detect(string = string,pattern = "it was okay") ~ TRUE,
            str_detect(string = string,pattern = "Awesome!") ~ TRUE,
            str_detect(string = string,pattern = "Hated it") ~ FALSE,
            str_detect(string = string,pattern = "Never read a better book") ~ TRUE,
            str_detect(string = string,pattern = "OK") ~ TRUE,
            str_detect(string = string,pattern = "The author's other books were better") ~ FALSE,
            str_detect(string = string,pattern = "A lot of material was not needed") ~ FALSE,
            str_detect(string = string,pattern = "Would not recommend") ~ FALSE,
            str_detect(string = string,pattern = "I learned a lot") ~ TRUE
            )
}


dataset <- dataset %>% 
           mutate(pos_review = pos_neg(user_submitted_review))
                  
dataset

dataset <- dataset %>% 
             mutate(date = mdy(date),
                    program_period = if_else(date >= '2019-07-1',TRUE,FALSE))

head(dataset,10)


## Data Analysis

dataset %>%
  group_by(program_period) %>% 
  summarize(total_books_purchased = sum(total_purchased_complete))

dataset %>% 
   group_by(program_period,customer_type) %>% 
  summarize(total_books_purchased = sum(total_purchased_complete))

dataset %>% 
  group_by(program_period) %>% 
  summarize(positive_reviews = sum(pos_review))

