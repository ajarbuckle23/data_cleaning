##### GENERAL ####
# Loading Packages 
library(pacman)
p_load(tidyr, dplyr)

# Getting data from desktop
orig_data <- read.csv("/Users/ajarbuckle/Desktop/Data Grind/Datasets/USA Rent/USA_rent.csv")

# Renaming columns 
orig_data <- orig_data %>%
  rename(
    B1.P = X1.Bed.Price,
    B1.MoM = X1.Bed.M.M.,
    B1.YoY = X1.Bed.Y.Y.,
    B2.P = X2.Bed.Price,
    B2.MoM = X2.Bed.M.M.,
    B2.YoY = X2.Bed.Y.Y.,
    B1.P.rank = Ranking,
    B1.P.c.rank = Ranking.Change
  )

# Splitting up table into four parts
rank_data <- orig_data %>%
  select(City, B1.P.rank, B1.P.c.rank)
price_data <- orig_data %>%
  select(City, B1.P, B2.P)
change_data1 <- orig_data %>%
  select(City, B1.MoM, B1.YoY)
change_data2 <- orig_data %>%
  select(City, B2.MoM, B2.YoY)

#### CONVERTING CHANGE TABLES TO LONG FORMAT AND UNITING THEM ####

# Giving them a number of bedrooms column
change_data1$bedrooms <- 1
change_data2$bedrooms <- 2
# Changing column names
change_data1 <- change_data1 %>% 
  rename(
    MoM = B1.MoM,
    YoY = B1.YoY
  )
change_data2 <- change_data2 %>% 
  rename(
    MoM = B2.MoM,
    YoY = B2.YoY
  )
# Combining them into one dataframe
change_data <- rbind(change_data1, change_data2)
# Dropping change1 and change2 dataframes
rm(change_data1, change_data2)


#### CONVERTING PRICE TABLE TO LONG FORMAT ####

price_data <- gather(price_data, bedrooms, price, B1.P:B2.P, factor_key=TRUE)
# Changing variable values 
# First have to change data type from factor to character
price_data$bedrooms <- as.character(price_data$bedrooms)
for (x in 1:nrow(price_data)){
  if(price_data[x,"bedrooms"] == "B1.P"){
    price_data[x,"bedrooms"] <- "1"
  }
  if(price_data[x,"bedrooms"] == "B2.P"){
    price_data[x,"bedrooms"] <- "2"
  }
}
# Now change it back to factor
price_data$bedrooms <- as.factor(price_data$bedrooms)

#### COMBNINING PRICE AND CHANGE DATAFRAMES INTO ONE ####
price_data <- cbind(change_data, price_data)
# Removing extra column names 
price_data <- price_data[c(1,4,7,2,3)]
# Dropping the change dataframe
rm(change_data)

# CHANGING DATA TYPES ####
# Removing $ from price column
price_data$price = gsub("\\$", "", price_data$price)
# Removing commas from price column 
price_data$price <- gsub(",","",price_data$price)
# Changing price column to numeric form 
price_data$price <- as.numeric(price_data$price)
# Removing % from MoM and YoY columns
price_data$MoM <- gsub("%","",price_data$MoM)
price_data$YoY <- gsub("%","",price_data$YoY)
# Converting MoM and YoY columns to numeric form
price_data$MoM <- as.numeric(price_data$MoM)
price_data$YoY <- as.numeric(price_data$YoY)

#### EXPORTING ####
write.csv(rank_data,"/Users/ajarbuckle/Desktop/Data Grind/Datasets/USA Rent/rank_data.csv", row.names = TRUE)
write.csv(price_data,"/Users/ajarbuckle/Desktop/Data Grind/Datasets/USA Rent/price_data.csv", row.names = TRUE)
