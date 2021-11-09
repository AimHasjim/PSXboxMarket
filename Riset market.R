library(twitteR)
library(ROAuth)
library(dplyr)
library(ggplot2)
consumer_key <- "Jangan liat sinii"
consumer_secret <- "Ihh kepo dehh"
access_token <- "hayo tebak hayoo"
access_secret <- "ee ngerik yaaa"
setup_twitter_oauth(consumer_key = consumer_key, 
                    consumer_secret = consumer_secret, access_token = access_token, 
                    access_secret = access_secret)

#### Followers ####
playstation <- getUser("@PlayStation")
xbox <- getUser("@Xbox")
datcon <- data.frame(Console = c("Playstation", "Xbox"), 
                     `Twitter Followers` = c(playstation$followersCount, xbox$followersCount))
datcon %>% ggplot(aes(Console, Twitter.Followers)) + 
  geom_bar(stat = "identity", fill = "#458b74") +
  scale_y_continuous(breaks = c(0, 5000000,10000000,15000000)) + 
  geom_text(data = labelfol, aes(x, y, label = TW), size = 6, colour = "#93e993") + 
  ylab("Pengikut")







#### Exclusive Games #####
sony <- c("Astro + Playroom", "Gran + Turismo + 7", "Horizon + Forbidden + West", "Spiderman + Miles + Morales", "Ratchet + clank + Rift + Apart", "Returnal", "Sackboy")
microsoft <- c("halo + infinite +xbox", "scorn + Xbox", "The + Ascent + Xbox", "Call + of + the + Sea + Xbox", "The + Medium + Xbox")
algo <- function(strings){searchTwitter(strings, n = 10000) %>% twListToDF()}
## Sony ##
twsony1 <- lapply(sony, FUN = algo)
num_of_tweets <- NA
for(i in 1:7){
  s <- twsony1[i] %>% as.data.frame() %>% .$text %>% length()
  num_of_tweets[i] <- s
}
sotweets <- data.frame(Games = sogame, Tweets = num_of_tweets)
sotweets %>% mutate(Games = reorder(Games, Tweets)) %>% ggplot(aes(Games, Tweets)) + geom_bar(stat = "identity",  fill = "#003791") + geom_text(data = labbs, aes(x, y, label = Tweets))

## Microsoft ###
twmicro <- lapply(microsoft, FUN = algo)
for(i in 1:5){
  s <- twmicro[i] %>% as.data.frame() %>% .$text %>% length()
  num_of_tweets[i] <- s
}
mitweets <- data.frame(Games = migame, Tweets = num_of_tweets)
mitweets %>% mutate(Games = reorder(Games, Tweets)) %>% ggplot(aes(Games, Tweets)) + geom_bar(stat = "identity",  fill = "#107C10") + geom_text(data = labb, aes(x, y, label = Tweets))
