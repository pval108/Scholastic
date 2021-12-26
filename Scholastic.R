library(dplyr)
library(tidyr)
library(plyr)


df <- read.csv('C:/Users/PRM/Documents/Projects/R/Scholastic/Scholastic_Challenge_Dataset/Scholastic_Challenge_Dataset.csv')
#https://www.dropbox.com/s/4r7qfkt9egctigu/Scholastic_Challenge_Dataset.zip?dl=0
#https://www.dropbox.com/s/ythqeee2h3om4xg/Data%20Dictionary.xlsx?dl=0


################################### DATA CLEANING ################################### 

#formatting blanks
df <- df %>% mutate_all(na_if,"")
#handling null values
df$STATE [df$STATE == ""] <- NA
df$ZIP_CODE [df$ZIP_CODE == "."] <- NA
df <- df[!is.na(df$STATE),]
df <- df[!is.na(df$ZIP_CODE),]
df$UNIT_PRICE[is.na(df$UNIT_PRICE)] <- mean(df$UNIT_PRICE, na.rm = TRUE)

#cleaning numerical values
df <- df[df$UNIT_PRICE <= 41,]
df$UNIT_PRICE <- ifelse(df$total_units <1,0,df$UNIT_PRICE)
df$revenue <-df$total_units * df$UNIT_PRICE

#formatting data types
df$total_units<-as.integer(df$total_units)
df$UNIT_PRICE <- round(df$UNIT_PRICE,2)
df$revenue <- round(df$revenue,2)

#filtering out free books
df<- df[df['UNIT_PRICE']>0,]

#formatting marketting channels
df$CH1_GENRE<-gsub("'","",df$CH1_GENRE)
df$CH1_THEME<-gsub("'","",df$CH1_THEME)
df$CH1_GENRE <-gsub("\\[|\\]", "", df$CH1_GENRE)
df$CH1_THEME <-gsub("\\[|\\]", "", df$CH1_THEME)

################################### DATA ORGANIZATION ################################### 

#splitting county data
bksales_county <- df %>%
  group_by(title,COUNTY,STATE) %>% #look into variables SERIES,CH1_GENRE,CH1_THEME,CH2_CATEGORY,CH2_SUBCATEGORY
  dplyr::summarize(units=sum(total_units))

#book prices averaged out - splitting book data
df.books <- df %>% 
  group_by(title) %>%
  dplyr::summarise(price=mean(UNIT_PRICE),units=sum(total_units))

#rounding book price
df.books$price <- round(df.books$price,2)
df.books$revenue<-df.books$price * df.books$units

#joining both dataframes
#x<- c('title','COUNTY','STATE','CHANNEL')
bksales_county <- join(bksales_county, df.books[c('title','price')],by='title',
                       type = "inner")

#calculating revenue
bksales_county$revenue <- bksales_county$units * bksales_county$price

#county level
df.county <- bksales_county %>%
  group_by(COUNTY,STATE) %>%
  dplyr::summarise(min=min(revenue),median=median(revenue),max=max(revenue),avg=mean(revenue),sum=sum(revenue),units=sum(units)) 

#state level
df.state <- bksales_county %>%
  group_by(STATE) %>%
  dplyr::summarise(min=min(revenue),median=median(revenue),max=max(revenue),avg=mean(revenue),sum=sum(revenue),units=sum(units)) 

#book level
df.books <- df %>%
  group_by(title) %>% #look into variables SERIES,CH1_GENRE,CH1_THEME,CH2_CATEGORY,CH2_SUBCATEGORY
  dplyr::summarize(units=sum(total_units),price=mean(UNIT_PRICE))



################################### TEXT MINING PORTION ################################### 

#marketing channel (text mining)
df.channel1 <- df[(!is.na(df$CH1_GENRE) | !is.na(df$CH1_THEME)) & df$CHANNEL == 'CHANNEL 1', ]
df.channel2 <- df[(!is.na(df$CH2_CATEGORY) | !is.na(df$CH2_SUBCATEGORY)) & df$CHANNEL == 'CHANNEL 2', ]

df.channel1 <- df.channel1 %>%
  group_by(CHANNEL,CH1_THEME,CH1_GENRE) %>%
  dplyr::summarise(min=min(revenue),median=median(revenue),max=max(revenue),avg=mean(revenue),sum=sum(revenue),units=sum(total_units)) 

df.channel2 <- df.channel2 %>%
  group_by(CHANNEL,CH2_CATEGORY,CH2_SUBCATEGORY) %>%
  dplyr::summarise(min=min(revenue),median=median(revenue),max=max(revenue),avg=mean(revenue),sum=sum(revenue),units=sum(total_units)) 

# the marketing channels are by high school ** create a county level dataframe
#limit f_set to county level
dim(unique(f_set))

#grouping by marketing channels?