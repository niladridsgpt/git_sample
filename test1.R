library(ggplot2)
library(dplyr)
        
#Find Prime numbers Bet0ween 1 and 1,000,000

library("numbers")
Prime_list =  as.data.frame(Primes(1000000)) 

# Question 2-3

#In Table 1, how would you find the average monthly scripts per ID?

library(readxl)
library(dplyr)

#importing data
table1 = read_excel("C:/Users/niladri.dasgupta/Downloads/table1.xlsx", 
                     col_types = c("text", "date", "text", 
                                   "numeric"))
View(table1)

#calculate the average monthly scripts

df = table1

df_grp_ID = df %>% group_by(ID)  %>%
  summarise(avg_scripts = mean(Scripts))

View(df_grp_ID)

#In Table 1, how would you compare an individual ID against others in the same ZIP code?


df_grp_ID2 = df %>% group_by(ZIP, ID) %>% summarise(total_scripts = sum(Scripts))

View(df_grp_ID2)

# Question 4-6
#In Table 2, how would you determine how many unique “Event Name” are in the table?

library(readxl)

#importing table2

table2 <- read_excel("C:/Users/niladri.dasgupta/Downloads/table2.xlsx", 
                     col_types = c("text", "text", "text"))
View(table2)

#Unique events

# Unique even name
Unique_event = unique(table2$`Event Name`)

#Number of Unique event
Num_unique_event =  n_distinct(table2$`Event Name`)
Num_unique_event

#In Table 2, how would you find all Events that correspond to the product “Taltz”?

Taltz_event = table2$`Event Name`[grepl('Taltz',table2$`Event Name` )]
Taltz_event

  #Alternative solution

    library('sqldf')
    Taltz_event= sqldf("select `Event Name` from table2 where upper(`Event Name`) like '%TALTZ%' ")

#Taltz is a medication that can be used to treat dermatological and rheumatological diseases. In Table 2, 
    #how would you find events that are related to dermatological vs rheumatological diseases? 

      #segregating Event name based on indication

      table2$event_indication <- ifelse(grepl("Derma", table2$`Event Name`, ignore.case = T), "Dermatological", 
                                  ifelse(grepl("Rheu", table2$`Event Name`, ignore.case = T), "Rheumatological", "Other"))


# Question-7
    
    #in table3 How do you calculate percentage increase in units sold compared to previous date for each product and date?

    #importing data
library(readxl)
table3 <- read_excel("C:/Users/niladri.dasgupta/Downloads/table3.xlsx", 
                     col_types = c("text", "text", "date", 
                                   "numeric"))
View(table3)

# Formatting the date column
daily_sales <- table3 %>%
  mutate(
    YearMonthDay = format(Date, "%Y-%m-%d")
  )

# Usual dplyr group_by() and summarize() sales by Id and Date

daily_sales <- daily_sales %>%
  group_by(`Product ID`,Date) %>%
  summarize(
    Total_sales = sum(`Units Sold`)
  ) %>%
  arrange(`Product ID`)

# Growth in Units sold for product compared to its previous date
daily_sales <- daily_sales %>%
  mutate(
    Growth = (Total_sales - lag(Total_sales)) / lag(Total_sales)
  )

# convert growth into percentage
daily_sales <- daily_sales %>%
  mutate(
    Growth = label_percent()(Growth)
  )


# Alternative way to find sales Growth compared to previous date

df= table3
df$DoD = sqldf('SELECT
        -1+(`Units Sold`/LAG(`Units Sold`,1)
        OVER (PARTITION BY `Product ID` ORDER BY Date ASC)) AS sales_percentage_growth
        FROM df')

#Question 8

#Using the data in Table 4 and an appropriate statistical model, how would you predict the weight of a new dog with characteristics: Breed = Labrador, Age = 4?
library(corrr)
library(tidyr)
library(ggplot2)
library(caret)
library("scales")

#importing data
table4 <- read_excel("C:/Users/niladri.dasgupta/Downloads/table4.xlsx", sheet = "Sheet1")
model_data=table4[-c(1)]

#Changing character variables to factors

model_data$Breed=as.factor(model_data$Breed)

# summary of date
summary(model_data)

#Imputed missing values

model_data[is.na(model_data)]=0

#Correlation analysis

res.cor <- correlate(model_data)

#Bi-Variate Analysis of weight vs age

plot(Weight ~ Age, data=model_data)

#Bi-Variate Analysis of weight vs Breed
ggplot(model_data, aes(Breed, Weight)) + 
  geom_point() + 
  labs(y = "Weight_dog", x = "Breed");

#Data Partition for modelling

set.seed(1234) 
training.samples <- 
  createDataPartition(model_data$Weight, p = 0.4, list = FALSE)
train <- model_data[training.samples, ]
test <- model_data[-training.samples, ]

#Creation of a Linear Regression model

lmtrain= lm(Weight~.,data = train)
print(lmtrain)
summary(lmtrain)

# Find weight of a Labrador dog of age 4 and 

a <- data.frame(Breed = 'Labrador', Age = 4)
result <-  predict(lmtrain,a)



