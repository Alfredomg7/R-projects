library(gtrendsR)
library(tidyverse)

#import Bitcoin and Ethereum price data to data frames
btc_data <- read_csv("BTC-USD.csv")
eth_data <- read_csv("ETH-USD.csv")

#Convert date column to date type
btc_data <- btc_data %>% 
  mutate(Date = as.Date(Date,"%d/%m/%Y"))

#Plot Bitcoin and Ethereum price from the last 5 years
ggplot(btc_data) + geom_line(aes(Date,Close), color="#00BA38") +
  labs(title="Bitcoin Market Price",
       caption="Data from Yahoo Finance", x=NULL, y="Market price") +
  scale_y_continuous(label = function(x) paste0('$', x))

ggplot(eth_data) + geom_line(aes(Date,Close), color="#619CFF") +
  labs(title="Ethereum Market Price",
       caption="Data from Yahoo Finance", x=NULL, y="Market price") +
  scale_y_continuous(label = function(x) paste0('$', x))

#Get Google trends data about crypto keywords from the last 5 years
crypto_trends <- gtrends(c("cryptocurrency", "blockchain", "NFT"))
crypto_trends_over_time <- crypto_trends$interest_over_time

#Change the "<1" values in "hits" column to 0 and convert to numeric data type
crypto_trends_over_time$hits[crypto_trends_over_time$hits == "<1"] <- "0"
crypto_trends_over_time <- crypto_trends_over_time %>% 
  mutate(hits=as.numeric(hits))

#Plot trends linear graph
ggplot(crypto_trends_over_time, aes(date, hits, color=keyword)) +
  geom_line() +
  scale_color_discrete(name = "Crypto Terms") + 
  labs(title = "Google Trend Data For Crypto Terms",
       x=NULL, y = "Hits (Normalized to be between 0 and 100)")

#Get top 10 countries by keyword
crypto_trends_countries <- crypto_trends$interest_by_country 
cryptocurrency_countries <- crypto_trends_countries %>% 
  filter(keyword=="cryptocurrency") %>% 
  arrange(-hits) %>% 
  head(10)

NFT_countries <- crypto_trends_countries %>% 
  filter(keyword=="NFT") %>% 
  arrange(-hits) %>% 
  head(10)
    
#Plot top 10 countries bar graph
ggplot(cryptocurrency_countries, aes(x=hits, 
      y=forcats::fct_reorder(location, hits))) + geom_col(fill="#00BA38") +
  labs(title="Top 10 countries most Interested in Cryptocurrency Keyword",
       x= "Hits (Normalized to be between 0 and 100)" , y=NULL)

ggplot(NFT_countries, aes(x=hits, 
      y=forcats::fct_reorder(location, hits))) + geom_col(fill="#619CFF") +
  labs(title="Top 10 countries most Interested in NFT Keyword",
       x= "Hits (Normalized to be between 0 and 100)" , y=NULL)

#Plot price and trends together for Bitcoin and cryptocurrency 
ggplot() + geom_line(btc_data, mapping = aes(Date, Close, 
  color = "Bitcoin Price")) + geom_line(filter(crypto_trends_over_time, 
  keyword == "cryptocurrency"), mapping = aes(as.Date(date), hits*1200,
  color = "Cryptocurrency Google Trends Hits")) +
  scale_y_continuous(name = "Price", sec.axis = sec_axis(~./1200,
  name="Google Trends Hits")) + 
  labs(title = "Bitcoin Price and Cryptocurrency Google Trends", x=NULL)

#Plot price and trends together for Ethereum and NFT 
ggplot() + geom_line(eth_data, mapping = aes(Date, Close, 
  color = "Ethereum Price")) + geom_line(filter(crypto_trends_over_time, 
  keyword == "NFT"), mapping = aes(as.Date(date), hits*50,
  color = "NFT Google Trends Hits")) +
  scale_y_continuous(name = "Price", sec.axis = sec_axis(~./50,
  name="Google Trends Hits")) + 
  labs(title = "Ethereum Price and NFT Google Trends", x=NULL)

#Merge Bitcoin and Ethereum price
crypto_price <- btc_data %>% 
  inner_join(eth_data, by="Date") %>% 
  select(Date,Close.x, Close.y )

#Plot Bitcoin and Ethereum price correlation
ggplot(crypto_price, aes(Close.x,Close.y)) + geom_point() +
  geom_smooth(method= "lm") +
  geom_smooth(se = FALSE, color = "green") +
  labs(title="Bitcoin vs Ethereum Price", 
       x="Bitcoin Price", y="Ethereum Price") +
  scale_y_continuous(label = function(x) paste0('$', x)) +
  scale_x_continuous(label = function(x) paste0("$", x))
  


  
